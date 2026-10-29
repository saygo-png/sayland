-- | Description : Tasty tests that explore the interleavings of a scenario with dejafu, and explain the ones that fail.
--
-- A failing interleaving is replayed with @WAYLAND_DEBUG=1@, so the failure shows the messages sent and received in it
-- and which thread handled each, along with the order the threads ran in. With @RACE_TRACE@ set, it also shows every
-- step each thread took.
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
import Test.DejaFu (ProPredicate (..), Result (..), Settings, lsimplify, set, systematically, withSetup)
import Test.DejaFu.Conc (Scheduler (..), runConcurrent, showCondition)
import Test.DejaFu.SCT (runSCTWithSettings)
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

-- | Explain a failing interleaving: how it ended, the order the threads ran in, and the messages it logs.
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
  (_, replayed, _) <- runConcurrent replay memtype (Replay (ranThreads interleaving) [] Nothing) $ withSetup (setup logs) (\x -> logs.mainStarts >> act x)
  messages <- reverse <$> IO.readIORef written
  -- The first thread that did IO marked the start.
  let attributed = zip (drop 1 $ reverse replayed.writers) messages
      width = List.maximum (0 : (length . threadName . fst <$> attributed))
      line (thread, message) = "  " <> padRight width (threadName thread) <> "  " <> stripNewline message
  everyStep <- isJust <$> lookupEnv "RACE_TRACE"
  pure
    . List.unlines
    $ ["ended with: " <> either showCondition show outcome, "threads ran: " <> describeRuns interleaving, "messages:"]
    <> (line <$> attributed)
    <> (if everyStep then "steps:" : (step <$> zip (ranThreads interleaving) interleaving) else [])
    <> foldMap (\how -> ["(the replay of the interleaving went its own way: " <> how <> ")"]) replayed.diverged
  where
    padRight n text = text <> replicate (n - length text) ' '
    step (thread, (_, _, action)) = "  " <> threadName thread <> ": " <> show action
    stripNewline = reverse . dropWhile (== '\n') . reverse

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

-- | Where a replay is: the threads still to run, in order, the threads that did IO, newest first, and where the replay
-- went its own way, if it did.
data Replay = Replay {queue :: [ThreadId], writers :: [ThreadId], diverged :: Maybe String}

-- | Run the threads in the order of a trace, recording which thread did each IO: wrote each log line.
replay :: Scheduler Replay
replay = Scheduler $ \_ runnable _ r -> case r.queue of
  next : rest -> case List.lookup next (NE.toList runnable) of
    Just lookahead -> (Just next, r{queue = rest, writers = [next | lookahead == WillLiftIO] <> r.writers})
    Nothing ->
      let lost = show (length rest) <> " steps before the end, " <> threadName next <> " cannot run. Runnable: "
       in (Nothing, r{diverged = Just $ lost <> intercalate ", " (threadName . fst <$> NE.toList runnable)})
  [] -> (Nothing, r)

-- | The runs of threads in a trace, like @main (12 steps), preempted by client-loop (3 steps)@.
describeRuns :: Trace -> String
describeRuns interleaving = intercalate ", " $ describe <$> NE.groupBy ((==) `on` fst) (zip (ranThreads interleaving) interleaving)
  where
    describe steps@((thread, (decision, _, action)) :| _) =
      prefix decision <> name thread action <> " (" <> show (length steps) <> plural (length steps) <> ")"
    prefix :: Decision -> String
    prefix = \case
      SwitchTo _ -> "preempted by "
      _ -> ""
    name :: ThreadId -> ThreadAction -> String
    name thread = \case
      CommitIORef _ _ -> "memory commit"
      _ -> threadName thread
    plural :: Int -> String
    plural n = if n == 1 then " step" else " steps"

-- | The name a thread was forked with, or its number.
threadName :: ThreadId -> String
threadName (ThreadId (Id name n)) = fromMaybe (show n) name
