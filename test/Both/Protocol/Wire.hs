{-# LANGUAGE RequiredTypeArguments #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Description : Tests for the wire.
module Both.Protocol.Wire (tests) where

import Data.Binary.Get (runGetOrFail)
import Data.Binary.Put (runPutM)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Relude
import Sayland.Wire
import System.Posix (Fd (Fd))
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Sayland.Wire"
    [ testProperty "wl_int roundtrip" $ prop_roundtrip (type WlInt)
    , testProperty "wl_uint roundtrip" $ prop_roundtrip (type WlUInt)
    , testProperty "wl_fixed roundtrip" $ prop_roundtrip (type WlFixed)
    , testProperty "wl_string roundtrip" $ prop_roundtrip (type WlString)
    , testProperty "wl_array roundtrip" $ prop_roundtrip (type WlArray)
    , testProperty "wl_fd roundtrip" $ prop_roundtrip (type WlFd)
    , testProperty "wl_new_id roundtrip" $ prop_roundtrip (type WlNewId)
    , testProperty "getHeader reverses mkMessage" prop_header
    ]

prop_roundtrip :: forall a -> (WireFormat a, Eq a, Show a) => a -> Property
prop_roundtrip _type x = roundtrip x === Right x
  where
    -- Encode a value and decode it again. A value that leaves bytes behind did not encode itself.
    roundtrip :: (WireFormat a) => a -> Either String a
    roundtrip y = case runGetOrFail (runStateT wireGet fds) bytes of
      Left (_, _, err) -> Left err
      Right (rest, _, (decoded, _))
        | BSL.null rest -> Right decoded
        | otherwise -> Left "decoding left bytes unread"
      where
        (fds, bytes) = runPutM $ execStateT (wirePut y) []

prop_header :: WlUInt -> Word16 -> [Word8] -> Property
prop_header objectID opcode body =
  runGetOrFail getHeader (mkMessage objectID opcode payload)
    === Right (payload, fromIntegral headerSize, (objectID, opcode, size))
  where
    payload = BSL.pack body
    size = headerSize + fromIntegral (BSL.length payload)

instance Arbitrary WlInt where
  arbitrary = WlInt <$> arbitrary
  shrink (WlInt i) = WlInt <$> shrink i

instance Arbitrary WlUInt where
  arbitrary = WlUInt <$> arbitrary
  shrink (WlUInt w) = WlUInt <$> shrink w

instance Arbitrary WlFixed where
  arbitrary = WlFixed <$> arbitrary
  shrink (WlFixed f) = WlFixed <$> shrink f

{- | A wl_string is NUL terminated, so a payload containing a NUL is not
representable on the wire. Never generate one, and never shrink towards one.
-}
instance Arbitrary WlString where
  arbitrary = WlString . BS.pack <$> listOf (arbitrary `suchThat` (/= 0))
  shrink (WlString s) = [WlString $ BS.pack w | w <- shrink (BS.unpack s), 0 `notElem` w]

instance Arbitrary WlArray where
  arbitrary = WlArray . BS.pack <$> arbitrary
  shrink (WlArray a) = WlArray . BS.pack <$> shrink (BS.unpack a)

-- | Descriptors are never dereferenced here, only carried around as numbers.
instance Arbitrary WlFd where
  arbitrary = WlFd . Fd . fromIntegral <$> chooseInt (0, 1023)
  shrink _ = []

instance Arbitrary WlNewId where
  arbitrary = WlNewId <$> arbitrary <*> arbitrary <*> arbitrary
  shrink (WlNewId n v i) = [WlNewId n' v' i' | (n', v', i') <- shrink (n, v, i)]
