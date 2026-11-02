-- | Description : Tasty tests that explore the interleavings of a scenario with dejafu, and explain the ones that fail.
--
-- A failing interleaving is replayed with @WAYLAND_DEBUG=1@, so the failure shows each run of a thread in it, why the
-- run ended (preempted, blocked, ...), and the messages the thread sent and received during the run. With @RACE_TRACE@
-- set, it also shows every step each thread took.
module RaceTest (
  Scenario,
  scenario,
  Logs,
  logged,
  raceTest,
  raceTestWith,
  defaultRaceSettings,
  randomInterleavings,
) where

import Data.IORef qualified as IO
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Sayland.Kernel (KernelConfig (..))
import System.Random (mkStdGen)
import Systematic (runSCTWithSettings)
import Test.DejaFu (ProPredicate (..), Result (..), Settings, lsimplify, set, systematically, withSetup)
import Test.DejaFu.Conc (Scheduler (..), runConcurrent, showCondition)
import Test.DejaFu.Settings (MemType, defaultBounds, defaultMemType, fromWayAndMemType, ldiscard, lmemtype, lway, randomly)
import Test.DejaFu.Settings qualified as Settings
import Test.DejaFu.Types (Condition, Decision (..), Id (..), Lookahead (..), ThreadAction (..), ThreadId (..), Trace, initialThread)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Providers (IsTest (..), singleTest, testFailed, testPassed)
import TestPrelude hiding (ThreadId)

-- | How a scenario logs: not at all while dejafu explores it, every message when a failing interleaving is replayed.
--
-- The two must take the same steps, so the replay can follow the interleaving. They do: dejafu takes a step for 'pure',
-- so not logging a line (@when False ...@) is a step, as logging it is.
data Logs = Logs
  { configure :: KernelConfig -> KernelConfig
  , mainStarts :: ConcIO ()
  -- ^ Run once the setup is done, so the replay only shows what the explored part logs.
  }

-- | Apply the logging to the scenario's kernel config.
logged :: Logs -> KernelConfig -> KernelConfig
logged logs = logs.configure

-- | No logs.
silent :: Logs
silent = Logs{configure = id, mainStarts = pass}

type role Scenario representational

-- | A scenario: a setup, run once with each thread running until it blocks and then snapshotted, and the concurrent
-- part, whose interleavings dejafu explores. The setup makes the kernel, with its config passed through `logged`.
data Scenario a where
  Scenario :: (Logs -> ConcIO x) -> (x -> ConcIO a) -> Scenario a

-- | A scenario from its setup and its concurrent part.
scenario :: (Logs -> ConcIO x) -> (x -> ConcIO a) -> Scenario a
scenario = Scenario

-- | Systematic exploration, bounded by preemptions and fairness as dejafu does by default, under total store order.
-- It runs with "Systematic"'s fix, so it does not lose races to sleep sets.
-- Failing traces are simplified, so the explanation shows as few preemptions as dejafu can find.
defaultRaceSettings :: Settings IO a
defaultRaceSettings = set lsimplify True $ fromWayAndMemType (systematically defaultBounds) defaultMemType

-- | Explore that many random interleavings instead, for scenarios too big to explore systematically. The random
-- generator has a fixed seed, so each run tries the same ones.
randomInterleavings :: Int -> Settings IO a -> Settings IO a
randomInterleavings n = set lway $ randomly (mkStdGen 42) n

-- | A test that the predicate holds over the interleavings of the scenario.
raceTest :: (Show b) => TestName -> ProPredicate a b -> Scenario a -> TestTree
raceTest = raceTestWith defaultRaceSettings

-- | `raceTest` with settings for dejafu.
raceTestWith :: (Show b) => Settings IO a -> TestName -> ProPredicate a b -> Scenario a -> TestTree
raceTestWith settings name predicate s = singleTest name $ RaceTest settings predicate s

data RaceTest where
  RaceTest :: (Show b) => Settings IO a -> ProPredicate a b -> Scenario a -> RaceTest

instance IsTest RaceTest where
  testOptions = pure []

  run _ (RaceTest settings predicate s@(Scenario setup act)) _ = do
    let discarding = set ldiscard (Just $ pdiscard predicate) settings
    result <- peval predicate <$> runSCTWithSettings discarding (withSetup (setup silent) (\x -> silent.mainStarts >> act x))
    case result._failures of
      _ | result._pass -> pure $ testPassed ""
      [] -> pure . testFailed $ "no interleaving failed, but the predicate did not hold: " <> result._failureMsg
      failure : more -> do
        explanation <- explain (Settings.get lmemtype settings) s failure
        pure
          . testFailed
          . List.unlines
          $ [result._failureMsg | not (null result._failureMsg)]
          <> [explanation]
          <> ["(" <> show (length more) <> " more failing interleavings not shown)" | not (null more)]

-- | Explain a failing interleaving: how it ended, and each run of a thread in it, with the messages it sent, received
-- and logged, and why the run ended.
explain :: (Show b) => MemType -> Scenario a -> (Either Condition b, Trace) -> IO String
explain memtype (Scenario setup act) (outcome, interleaving) = do
  started <- IO.newIORef False
  written <- IO.newIORef []
  let sink line = liftIO $ IO.readIORef started >>= \yes -> when yes $ IO.modifyIORef written (line :)
      logs =
        Logs
          { configure = \config -> config{environment = config.environment <> [("WAYLAND_DEBUG", "1")], stderr = sink}
          , mainStarts = liftIO $ IO.writeIORef started True
          }
  (_, replayed, _) <- runConcurrent replay memtype (Replay (ranThreads interleaving) 0 [] Nothing) $ withSetup (setup logs) (\x -> logs.mainStarts >> act x)
  messages <- reverse <$> IO.readIORef written
  -- The first step that did IO marked the start, the others each wrote a line.
  let logLines = zip (drop 1 . reverse $ fst <$> replayed.writers) messages
  everyStep <- isJust <$> lookupEnv "RACE_TRACE"
  pure
    . List.unlines
    $ ["ended with: " <> either showCondition show outcome, "interleaving:"]
    <> describeRuns interleaving logLines
    <> (if everyStep then "steps:" : (step <$> zip (ranThreads interleaving) interleaving) else [])
    <> foldMap (\how -> ["(the replay of the interleaving went its own way: " <> how <> ")"]) replayed.diverged
  where
    step (thread, (_, _, action)) = "  " <> threadName thread <> ": " <> show action

-- | The thread that ran each step of a trace.
ranThreads :: Trace -> [ThreadId]
ranThreads = go initialThread
  where
    go previous = \case
      [] -> []
      (decision, _, _) : rest ->
        let thread = case decision of
              Start t -> t
              SwitchTo t -> t
              Continue -> previous
         in thread : go thread rest

-- | Where a replay is: the threads still to run, in order, how many steps ran, the steps that did IO with their thread,
-- newest first, and where the replay went its own way, if it did.
data Replay = Replay {queue :: [ThreadId], ran :: Int, writers :: [(Int, ThreadId)], diverged :: Maybe String}

-- | Run the threads in the order of a trace, recording the steps that did IO: wrote a log line.
replay :: Scheduler Replay
replay = Scheduler $ \_ runnable _ r -> case r.queue of
  next : rest -> case List.lookup next (NE.toList runnable) of
    Just lookahead -> (Just next, r{queue = rest, ran = r.ran + 1, writers = [(r.ran, next) | lookahead == WillLiftIO] <> r.writers})
    Nothing ->
      let lost = show (length rest) <> " steps before the end, " <> threadName next <> " cannot run. Runnable: "
       in (Nothing, r{diverged = Just $ lost <> intercalate ", " (threadName . fst <$> NE.toList runnable)})
  [] -> (Nothing, r)

-- | Each run of a thread in a trace, with why it ended, and the lines it logged under it, given with their step:
--
-- > -- main runs 45 steps, until server-client-1 preempts it
-- >      -> wl_display#1.sync { callback: FromClient 4 }
describeRuns :: Trace -> [(Int, String)] -> [String]
describeRuns interleaving logLines = concat $ zipWith describe runs (drop 1 (Just <$> runs) <> [Nothing])
  where
    runs = NE.groupBy ((==) `on` (.thread)) $ List.zipWith3 (\i t (d, _, a) -> Step i t d a) [0 ..] (ranThreads interleaving) interleaving
    describe steps next =
      let start = NE.head steps
          count = length steps
          inRun = [stripLevel line | (n, line) <- logLines, n >= start.index, n <= (NE.last steps).index]
       in ("  -- " <> name start <> " runs " <> show count <> (if count == 1 then " step" else " steps") <> ending steps next)
            : (("       " <>) <$> inRun)
    ending steps = \case
      Just next | SwitchTo t <- (NE.head next).decision -> ", until " <> threadName t <> " preempts it"
      _ -> case show (NE.last steps).action of
        action
          | "Blocked" `List.isPrefixOf` action -> ", until it blocks"
          | "Stop" `List.isPrefixOf` action -> ", until it ends"
          | "Throw" `List.isPrefixOf` action -> ", until it throws"
          | otherwise -> ""
    name step = case step.action of
      CommitIORef _ _ -> "a memory commit"
      _ -> threadName step.thread
    -- The messages are at DEBUG level, so only the other levels are worth showing.
    stripLevel line = let bare = stripNewline line in maybe bare (dropWhile (== ' ')) $ List.stripPrefix "DEBUG" bare
    stripNewline = reverse . dropWhile (== '\n') . reverse

-- | A step of a trace: its index, the thread that took it, how it was scheduled and what it did.
data Step = Step {index :: Int, thread :: ThreadId, decision :: Decision, action :: ThreadAction}

-- | The name a thread was forked with, or its number.
threadName :: ThreadId -> String
threadName (ThreadId (Id name n)) = fromMaybe (show n) name
