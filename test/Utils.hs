module Utils (module Utils) where

import Relude
import Test.Tasty.HUnit

-- | Lifted version of `@?=`.
(@?==) :: (HasCallStack, MonadIO m, Eq a, Show a) => a -> a -> m ()
actual @?== expected = liftIO (actual @?= expected)

-- | Run the action and check what it returns.
(>>=@?==) :: (HasCallStack, MonadIO m, Eq a, Show a) => m a -> a -> m ()
action >>=@?== expected = action >>= (@?== expected)

infix 1 @?==, >>=@?==
