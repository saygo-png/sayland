-- | Description : Tests for merging content updates.
module ContentUpdate (tests) where

import Relude
import Sayland
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "ContentUpdate"
    [ testCase "the later update's state replaces the earlier's" laterReplaces
    , testCase "state the later update does not set is kept" unsetKept
    , testCase "offsets add up" offsetsAdd
    , testCase "the damage of both updates is kept" damageKept
    , testCase "apply hooks of both run, the earliest first" applyHooksInOrder
    ]

-- | An update that changes nothing for setting fields
-- using `mempty` triggers hlint rules for monoid laws.
noChange :: ContentUpdate
noChange = mempty

laterReplaces :: Assertion
laterReplaces = (noChange{cuBufferScale = Just 2} <> mempty{cuBufferScale = Just 3}).cuBufferScale @?= Just 3

unsetKept :: Assertion
unsetKept = do
  (noChange{cuBufferScale = Just 2} <> noChange).cuBufferScale @?= Just 2
  (noChange <> noChange{cuBufferScale = Just 2}).cuBufferScale @?= Just 2

-- | An offset is relative to the previous buffer, so two in a row move the buffer by their sum.
offsetsAdd :: Assertion
offsetsAdd = (noChange{cuOffset = Just (1, 2)} <> noChange{cuOffset = Just (10, 20)}).cuOffset @?= Just (11, 22)

damageKept :: Assertion
damageKept = sort ((.position) <$> merged.cuDamage) @?= [(1, 1), (2, 2)]
  where
    merged = noChange{cuDamage = [damage 1]} <> noChange{cuDamage = [damage 2]}
    damage n = Rectangle{position = (n, n), size = (1, 1)}

applyHooksInOrder :: Assertion
applyHooksInOrder = do
  ran <- newTVarIO []
  let hook name = modifyTVar' ran (<> [name :: Text])
  atomically . sequenceA_ $ (noChange{cuOnApply = [hook "earlier"]} <> noChange{cuOnApply = [hook "later"]}).cuOnApply
  readTVarIO ran >>= (@?= ["earlier", "later"])
