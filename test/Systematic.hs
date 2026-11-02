-- | Description : dejafu's systematic exploration, fixed so it does not lose races to sleep sets.
--
-- dejafu explores interleavings with dynamic partial-order reduction (DPOR): it runs one interleaving, finds the steps
-- of other threads that the steps in it race with, and tries those threads earlier. It also keeps sleep sets: a thread
-- whose step was already tried at a point sleeps until a step it does not commute with runs.
--
-- The two disagree on transactions. DPOR only knows a thread's next step is "a transaction", so it takes it to race
-- with every earlier transaction, and tries the thread before the last of them. The sleep sets use the 'TVar's the
-- transactions touched. So DPOR can try a thread before a transaction it does not touch the 'TVar's of, which stays
-- asleep; once nothing else can run, dejafu aborts the execution, as one it already covered. dejafu also drops the
-- races it found in the execution, and the race with the transaction before, the one that matters, is never tried.
--
-- Say thread A looks an object up, then forgets it, then sends a message, all in transactions, and B looks it up too.
-- The first run has A do all three before B. B's lookup races with A's send, so dejafu tries B before the send. A's
-- send sleeps, B's lookup finds nothing, and dejafu aborts the run without noting that B's lookup also races with A's
-- forget, so it never tries B there, where both threads find the object.
--
-- `runSCTWithSettings` here is dejafu's, except it keeps the races found in aborted executions, as the DPOR algorithm
-- dejafu implements (Flanagan and Godefroid's) does: it finds races as each step runs, whether or not the execution
-- ends up blocked by its sleep set.
module Systematic (runSCTWithSettings) where

import Data.Map.Strict qualified as Map
import Test.DejaFu.Conc (Condition, Program)
import Test.DejaFu.Internal (Way (..))
import Test.DejaFu.SCT qualified as DejaFu
import Test.DejaFu.SCT.Internal (sct)
import Test.DejaFu.SCT.Internal.DPOR
import Test.DejaFu.Settings (Settings, get, llengthBound, lmemtype, lsafeIO, lway)
import Test.DejaFu.Types (Bounds (..), Decision (..), FairBound (..), LengthBound (..), Lookahead (..), PreemptionBound (..), ThreadAction (..), Trace, initialThread)
import Test.DejaFu.Types qualified as DejaFu (ThreadId)
import Test.DejaFu.Utils (tidOf)
import TestPrelude hiding (ThreadId, get)

-- | Explore the interleavings of a program as dejafu's own does, finding the races aborted executions show too.
runSCTWithSettings :: Settings IO a -> Program pty IO a -> IO [(Either Condition a, Trace)]
runSCTWithSettings settings = case get lway settings of
  Systematic bounds -> sct settings initialState findSchedulePrefix (step bounds)
  Randomly{} -> DejaFu.runSCTWithSettings settings
  where
    safeIO = get lsafeIO settings
    memtype = get lmemtype settings
    step bounds cstate0 run dpor (prefix, conservative, sleep) = do
      (result, s, interleaving) <- run (dporSched safeIO (cBound (get llengthBound settings) bounds)) (initialDPORSchedState sleep prefix cstate0)
      let backtracks = findBacktrackSteps safeIO memtype (cBacktrack bounds) (schedBoundKill s) cstate0 (schedBPoints s) interleaving
          dpor' = incorporateBacktrackSteps backtracks $ incorporateTrace safeIO memtype conservative interleaving cstate0 dpor
      -- dejafu drops the backtracks too when it ignores the execution.
      pure (force dpor', if schedIgnore s then Nothing else Just (result, interleaving))

-- The rest is copied from dejafu's "Test.DejaFu.SCT", which does not export it.

-- | Combination bound function.
cBound :: Maybe LengthBound -> Bounds -> IncrementalBoundFunc ((Int, Maybe DejaFu.ThreadId), Map DejaFu.ThreadId Int, Int)
cBound lb (Bounds pb fb) (Just (k1, k2, k3)) prior lh =
  let k1' = maybe (\k _ _ -> k) pBound pb (Just k1) prior lh
      k2' = maybe (\k _ _ -> k) fBound fb (Just k2) prior lh
      k3' = maybe (\k _ _ -> k) lBound lb (Just k3) prior lh
   in (,,) <$> k1' <*> k2' <*> k3'
cBound _ _ Nothing _ _ = Just ((0, Nothing), Map.empty, 1)

-- | Backtracks to the given point.
--
-- If pre-emption bounding is enabled, also conservatively adds a backtracking point prior to the most recent
-- transition before that point. This may result in the same state being reached multiple times, but is needed because
-- of the artificial dependency imposed by the bound.
cBacktrack :: Bounds -> BacktrackFunc
cBacktrack (Bounds (Just _) _) bs =
  backtrackAt (\_ _ -> False) bs . concatMap addConservative
  where
    addConservative o@(i, _, tid) =
      o : case conservative i of
        Just j -> [(j, True, tid)]
        Nothing -> []

    -- index of conservative point
    conservative i = go (reverse (take (i - 1) bs)) (i - 1)
      where
        go _ (-1) = Nothing
        go (b1 : rest@(b2 : _)) j
          | bcktThreadid b1
              /= bcktThreadid b2
              && not (isCommitRef $ bcktAction b1)
              && not (isCommitRef $ bcktAction b2) =
              Just j
          | otherwise = go rest (j - 1)
        go _ _ = Nothing
cBacktrack _ bs = backtrackAt (\_ _ -> False) bs

-- | Pre-emption bound function. This does not count pre-emptive context switches to a commit thread.
pBound :: PreemptionBound -> IncrementalBoundFunc (Int, Maybe DejaFu.ThreadId)
pBound (PreemptionBound pb) k prior lhead =
  let k'@(pcount, _) = preEmpCountInc (fromMaybe (0, Nothing) k) prior lhead
   in if pcount <= pb then Just k' else Nothing

-- | Fair bound function
fBound :: FairBound -> IncrementalBoundFunc (Map DejaFu.ThreadId Int)
fBound (FairBound fb) k prior lhead =
  let k' = yieldCountInc (fromMaybe Map.empty k) prior lhead
   in if not (willYield (snd lhead)) || maxDiff (Map.elems k') <= fb
        then Just k'
        else Nothing

-- | Length bound function
lBound :: LengthBound -> IncrementalBoundFunc Int
lBound (LengthBound lb) len _ _ =
  let len' = maybe 1 (+ 1) len
   in if len' < lb then Just len' else Nothing

-- | An incremental version of 'preEmpCount', going one step at a time.
preEmpCountInc :: (Int, Maybe DejaFu.ThreadId) -> Maybe (DejaFu.ThreadId, ThreadAction) -> (Decision, a) -> (Int, Maybe DejaFu.ThreadId)
preEmpCountInc (sofar, lastnoncommit) prior (d, _) = case (prior, d) of
  (Just (tid, _), Start tnext) -> cswitch tid tnext False
  (Just (tid, act), SwitchTo tnext) -> cswitch tid tnext (not (didYield act))
  (_, Continue) -> (sofar, lastnoncommit)
  (Nothing, _) -> (sofar, lastnoncommit)
  where
    cswitch tid tnext isPreemptive
      | isCommitThread tnext = (sofar, if isCommitThread tid then lastnoncommit else Just tid)
      | isCommitThread tid = (if lastnoncommit == Just tnext then sofar else sofar + 1, Nothing)
      | otherwise = (if isPreemptive then sofar + 1 else sofar, Nothing)

    isCommitThread = (< initialThread)

-- | An incremental count of yields, going one step at a time.
yieldCountInc :: Map DejaFu.ThreadId Int -> Maybe (DejaFu.ThreadId, a) -> (Decision, Lookahead) -> Map DejaFu.ThreadId Int
yieldCountInc sofar prior (d, lnext) = case prior of
  Just (tid, _) -> ycount (tidOf tid d)
  Nothing -> ycount initialThread
  where
    ycount tnext
      | willYield lnext = Map.alter (Just . maybe 1 (+ 1)) tnext sofar
      | otherwise = Map.alter (Just . fromMaybe 0) tnext sofar

-- | Determine if an action is a commit or not.
isCommitRef :: ThreadAction -> Bool
isCommitRef (CommitIORef _ _) = True
isCommitRef _ = False

-- | Get the maximum difference between two ints in a list.
maxDiff :: [Int] -> Int
maxDiff = go 0
  where
    go m (x : xs) =
      let m' = m `max` foldl' (go' x) 0 xs
       in go m' xs
    go m [] = m
    go' x0 m x = m `max` abs (x0 - x)
