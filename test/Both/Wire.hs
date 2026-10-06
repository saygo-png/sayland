{-# LANGUAGE RequiredTypeArguments #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Description : Tests for the wire.
module Both.Wire (tests) where

import Data.Binary.Get (runGetOrFail)
import Data.Binary.Put (putWord32le, runPut, runPutM)
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
    , testProperty "getHeader reverses putHeader" prop_header
    , testProperty "decodeMessage reverses encodeMessage" prop_message
    , testProperty "decodeMessage only returns non-partial messages" prop_decodePartial
    , testProperty "decodeMessage does not decode a partial header" prop_decodePartialHeader
    , testProperty "decodeMessage rejects sizes smaller than the header" prop_decodeTooSmall
    , testProperty "decodeMessage rejects sizes that are not a multiple of 4" prop_decodeUnaligned
    , wlStringTests
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
  runGetOrFail getHeader (BS.fromStrict (encodeMessage objectID opcode payload))
    === Right (BS.fromStrict payload, fromIntegral headerSize, (objectID, opcode, size))
  where
    payload = BS.pack body
    size = headerSize + fromIntegral (BS.length payload)

prop_message :: WlUInt -> Word16 -> Body -> Property
prop_message objectID opcode (Body body) =
  decodeMessage encodedMsg === Right (objectID, opcode, body, "")
  where
    encodedMsg = encodeMessage objectID opcode body

{- | A message whose header has arrived but whose body has not fully arrived yet must not decode.
The test keeps only the first @kept@ bytes of a message, anywhere from just the header to all but the last byte.
-}
prop_decodePartial :: WlUInt -> Word16 -> Property
prop_decodePartial objectID opcode =
  forAllShrink (arbitrary `suchThat` hasBytes) (filter hasBytes . shrink) $ \(Body body) ->
    let encodedMsg = encodeMessage objectID opcode body
     in forAll (chooseInt (fromIntegral headerSize, BS.length encodedMsg - 1)) $ \kept ->
          decodeMessage (BS.take kept encodedMsg) === Left Incomplete
  where
    hasBytes (Body b) = not (BS.null b)

-- | Fewer bytes than a header can't decode, whatever they are.
prop_decodePartialHeader :: [Word8] -> Property
prop_decodePartialHeader bytes = decodeMessage (BS.pack (take (fromIntegral headerSize - 1) bytes)) === Left Incomplete

-- | A header claiming any size, unlike `encodeMessage` which derives it from the body.
header :: WlUInt -> Word16 -> Word16 -> BS.ByteString
header objectID opcode size = BSL.toStrict . runPut $ putHeader (objectID, opcode, size)

-- | A size smaller than the header itself is malformed, whatever follows it.
prop_decodeTooSmall :: WlUInt -> Word16 -> [Word8] -> Property
prop_decodeTooSmall objectID opcode trailing =
  forAll (chooseEnum (0, headerSize - 1)) $ \size ->
    decodeMessage (header objectID opcode size <> BS.pack trailing) === Left (InvalidSize $ SizeTooSmall size)

-- | Arguments are all multiples of 4 bytes, so a size that isn't is malformed.
prop_decodeUnaligned :: WlUInt -> Word16 -> Property
prop_decodeUnaligned objectID opcode =
  forAll (chooseEnum (headerSize, 256) `suchThat` \s -> s `mod` 4 /= 0) $ \size ->
    decodeMessage (header objectID opcode size <> BS.replicate (fromIntegral size) 0) === Left (InvalidSize $ SizeUnaligned size)

-- WlString {{{

wlStringTests :: TestTree
wlStringTests =
  testGroup
    "Sayland.Wire.WlString"
    [ testProperty "`WlString` must start with an unsigned 32-bit length including NUL terminator" prop_WlStringLengthCountsNul
    , testProperty "`WlString` must be UTF-8 encoded" prop_WlStringFromStringIsUtf8
    , testProperty "`WlString` must be NUL terminated" prop_WlStringNeedsNulTerminator
    , testProperty "`WlString` must not contain NUL inside" prop_WlStringNoEmbeddedNul
    ]

-- A string in wayland:
-- https://wayland.freedesktop.org/docs/book/Protocol.html#string

-- Starts with an unsigned 32-bit length (including null terminator)
prop_WlStringLengthCountsNul :: StringContents -> Property
prop_WlStringLengthCountsNul contents =
  BS.take 4 (encodeWire (WlString bytes)) === word32le (BS.length bytes + nullTerminatorLen)
  where
    bytes = encodeContents contents
    nullTerminatorLen :: Int = 1

-- followed by the UTF-8 encoded string contents
prop_WlStringFromStringIsUtf8 :: Property
prop_WlStringFromStringIsUtf8 = conjoin [isUtf8 "ł", isUtf8 "€", property isUtf8]
  where
    isUtf8 text = fromString text === WlString (encodeContents $ StringContents text)

-- including terminating null byte
prop_WlStringNeedsNulTerminator :: StringContents -> Property
prop_WlStringNeedsNulTerminator contents = not (BS.null bytes) ==> rejectsString bytes
  where
    bytes = encodeContents contents

-- TODO: then padding to a 32-bit boundary.

-- TODO: A null value is represented with a length of 0.

-- Interior null bytes are not permitted.
prop_WlStringNoEmbeddedNul :: StringContents -> StringContents -> Property
prop_WlStringNoEmbeddedNul prefix suffix =
  rejectsString (encodeContents prefix <> nulTerm <> encodeContents suffix <> nulTerm)

rejectsString :: BS.ByteString -> Property
rejectsString bytes = counterexample (show decoded) (isLeft decoded)
  where
    -- A WlArray can be a valid `WlString`, but it can also be invalid.
    -- We use this to test invalid `WlString`s one violation at a time.
    encoded = encodeWire $ WlArray bytes
    decoded = runWireGet (wireGet @WlString) [] encoded

encodeWire :: (WireFormat a) => a -> BS.ByteString
encodeWire = fst . runWirePut . wirePut

word32le :: Int -> BS.ByteString
word32le = BSL.toStrict . runPut . putWord32le . fromIntegral

-- | A NUL terminator byte
nulTerm :: BS.ByteString
nulTerm = BS.singleton 0x00

{- | Valid contents of a wl_string as a `String`. Characters without NUL, possibly none.
`contentBytes` turns the content into UTF-8 bytes.
-}
newtype StringContents = StringContents String deriving stock (Show)

instance Arbitrary StringContents where
  arbitrary = StringContents <$> listOf (arbitrary `suchThat` validChar)
  shrink (StringContents text) = StringContents <$> filter (all validChar) (shrink text)

-- | Encode StringContents into UTF-8
encodeContents :: StringContents -> BS.ByteString
encodeContents (StringContents s) = encodeUtf8 s

-- | Characters allowed in a wl_string.
validChar :: Char -> Bool
validChar c = c /= '\0' && notSurrogate c

-- | Unicode has fake characters called surrogates.
notSurrogate :: Char -> Bool
notSurrogate c = c < '\xD800' || c > '\xDFFF'

{- | A wl_string is NUL terminated, so a payload containing a NUL is not
representable on the wire. Never generate one, and never shrink towards one.
-}
instance Arbitrary WlString where
  arbitrary = WlString . encodeContents <$> arbitrary
  shrink (WlString s) = WlString . encodeContents <$> shrink (StringContents (decodeUtf8 s))

-- }}}

-- | A message body. Every argument is a multiple of 4 bytes, so a body is too.
newtype Body = Body BS.ByteString deriving stock (Show)

instance Arbitrary Body where
  arbitrary = chooseInt (0, 32) >>= fmap (Body . BS.pack) . vector . (* 4)
  shrink (Body b) = [Body (BS.take n b) | n <- [0, 4 .. BS.length b - 4]]

instance Arbitrary WlInt where
  arbitrary = WlInt <$> arbitrary
  shrink (WlInt i) = WlInt <$> shrink i

instance Arbitrary WlUInt where
  arbitrary = WlUInt <$> arbitrary
  shrink (WlUInt w) = WlUInt <$> shrink w

instance Arbitrary WlFixed where
  arbitrary = WlFixed <$> arbitrary
  shrink (WlFixed f) = WlFixed <$> shrink f

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

-- vim: foldmethod=marker
