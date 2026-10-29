-- | Description : Assertions for tests that run in t'ConcIO', and running such a test once.
module Utils (module Utils) where

import Control.Exception qualified as E
import GHC.Stack (SrcLoc)
import Test.DejaFu.Conc (Condition (..), MemType (..), roundRobinSchedNP, runConcurrent, showCondition, showTrace)
import Test.Tasty.HUnit
import TestPrelude

-- | Fail the test with a message. Like `assertFailure`, but thrown in t'ConcIO', so dejafu sees it.
failTest :: (HasCallStack, MonadConcIO m) => String -> m a
failTest = throwIO . HUnitFailure caller
  where
    caller :: Maybe SrcLoc
    caller = snd <$> viaNonEmpty last (getCallStack callStack)

-- | Like `assertBool`, in t'ConcIO'.
assertTrue :: (HasCallStack, MonadConcIO m) => String -> Bool -> m ()
assertTrue message ok = unless ok $ failTest message

-- | Like `@?=`, in t'ConcIO'.
(@?==) :: (HasCallStack, MonadConcIO m, Eq a, Show a) => a -> a -> m ()
actual @?== expected = unless (actual == expected) . failTest $ "expected: " <> show expected <> "\n but got: " <> show actual

-- | Run the action and check what it returns.
(>>=@?==) :: (HasCallStack, MonadConcIO m, Eq a, Show a) => m a -> a -> m ()
action >>=@?== expected = action >>= (@?== expected)

infix 1 @?==, >>=@?==

-- | Run a program once, letting each thread run until it blocks, then the next one. What it throws is rethrown, so
-- a failed assertion fails the test case. Deadlocks fail it with dejafu's trace.
runOnce :: ConcIO a -> IO a
runOnce program =
  runConcurrent roundRobinSchedNP SequentialConsistency () program >>= \case
    (Right a, _, _) -> pure a
    (Left (UncaughtException e), _, _) -> E.throwIO e
    (Left condition, _, interleaving) -> assertFailure $ showCondition condition <> "\n" <> showTrace interleaving
