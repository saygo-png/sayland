-- | Description : Decoding and encoding wire protocol values.
module Sayland.Internal.Wire (WlInt (..), WlUInt (..), WlFixed (..), WlString (..), WlArray (..), WlFd (..), WlNewId (..), WireGet, WirePut, runWireGet, runWirePut, decodeMessage, DecodeError (..), InvalidSize (..), BodyWords, parseBodyWords, bodySize, wireGet, wirePut, WireFormat, headerSize, waylandNull, getHeader, putHeader, encodeMessage, RawObjectID) where

import Control.Exception (Exception)
import Control.Monad.State.Strict (MonadTrans (lift), StateT)
import Control.Monad.State.Strict qualified as State
import Data.Bifunctor
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Functor
import Data.Int
import Data.String (IsString)
import System.Posix (Fd)
import Prelude

-- | WireGet monad, appends file descriptors to the Get monad.
type WireGet = StateT [Fd] Get

-- | WirePut monad, appends file descriptors to the PutM monad.
type WirePut = StateT [Fd] PutM

-- | Run a `WirePut`, returning the bytes and the file descriptors it put, in the order they were put.
runWirePut :: WirePut () -> (BS.ByteString, [Fd])
runWirePut p = (BSL.toStrict bytes, reverse fds) -- `WlFd` conses, so the state is newest first.
  where
    (fds, bytes) = runPutM (State.execStateT p [])

-- | Run a `WireGet`, taking file descriptors from the front of the list. Returns the ones it did not take.
runWireGet :: WireGet a -> [Fd] -> BS.ByteString -> Either String (a, [Fd])
runWireGet g fds bytes = case runGetOrFail (State.runStateT g fds) (BS.fromStrict bytes) of
  Left (_, _, err) -> Left err
  Right (_, _, result) -> Right result

-- | Typeclass for wire types implementing decoding and serializing.
class WireFormat a where
  -- | Get a value from the wire.
  wireGet :: WireGet a

  -- | Put a value from the wire.
  wirePut :: a -> WirePut ()

-- | Wayland @int@. 32-bit signed integer.
newtype WlInt = WlInt Int32 deriving newtype (Show, Eq, Ord, Num, Integral, Enum, Real)

-- | Wayland @uint@. 32-bit unsigned integer.
newtype WlUInt = WlUInt Word32 deriving newtype (Show, Eq, Ord, Num, Integral, Enum, Real)

-- | Wayland @fixed@. 24.8 bit signed fixed-point number.
newtype WlFixed = WlFixed Int32 deriving newtype (Show, Eq, Ord)

{- | Wayland @string@.
A string, prefixed with a 32-bit integer specifying its length (in bytes), followed by the string contents and a NUL terminator, padded to 32 bits with zero bytes. The encoding is not specified. The `isString` instance encodes in UTF-8.
-}
newtype WlString = WlString BS.ByteString deriving newtype (Show, Eq, Ord, IsString, Semigroup, Monoid)

{- | Wayland @object@. Equivalent to `WlUInt`.
Used internally, users should use `ObjectID` as it offers more safety guaranatees using the type system.
-}
type RawObjectID = WlUInt

{- | Wayland @array@.
A blob of arbitrary data, prefixed with a 32-bit integer specifying its length (in bytes), then the verbatim contents of the array, padded to 32 bits with zero bytes.
-}
newtype WlArray = WlArray BS.ByteString deriving newtype (Show, Eq, Ord)

{- | Wayland @fd@.
0-bit value on the primary transport, but transfers a file descriptor to the other end using the ancillary data in the Unix domain socket message (msg_control).
This is the reason why we don't use a `Binary` instance for wire types, as we need additional state to house fds.
-}
newtype WlFd = WlFd Fd deriving newtype (Show, Eq, Ord)

{- | Wayland @new_id@.
A 32-bit unspecified object ID. Preceded by a `WlString` specifying the interface name, and a `WlUInt` specifying the version.
-}
data WlNewId = WlNewId WlString WlUInt WlUInt deriving stock (Show, Eq)

instance WireFormat WlUInt where
  wireGet = WlUInt <$> lift getWord32le
  wirePut (WlUInt w) = lift (putWord32le w)

instance WireFormat WlInt where
  wireGet = WlInt <$> lift getInt32le
  wirePut (WlInt i) = lift (putInt32le i)

instance WireFormat WlFixed where
  wireGet = WlFixed <$> lift getInt32le
  wirePut (WlFixed f) = lift (putInt32le f)

instance WireFormat WlArray where
  wireGet = lift $ do
    len <- fromIntegral <$> getWord32le
    WlArray <$> getByteString len <* skip (padTo4 len)
  wirePut (WlArray bs) = lift $ do
    putWord32le (fromIntegral $ BS.length bs)
    putByteString bs
    putByteString (BS.replicate (padTo4 $ BS.length bs) 0)

-- A string is an array whose bytes end in NUL.
instance WireFormat WlString where
  wireGet = wireGet <&> \(WlArray raw) -> WlString (BS.takeWhile (/= 0) raw)
  wirePut (WlString s) = wirePut (WlArray (s <> "\0"))

instance WireFormat WlNewId where
  wireGet = WlNewId <$> wireGet <*> wireGet <*> wireGet
  wirePut (WlNewId n v i) = wirePut n >> wirePut v >> wirePut i

instance WireFormat WlFd where
  wireGet =
    State.get >>= \case
      [] -> fail "sayland: fd requested, none received"
      f : fs -> WlFd f <$ State.put fs
  wirePut (WlFd f) = State.modify' (f :)

{- | Number needed to round n up to the next multiple of 4.
Used to determine 0 byte padding for types such as `WlString` or `WlArray`.
-}
padTo4 :: Int -> Int
padTo4 n = negate n `mod` 4

-- | The header size is always 8 in Wayland.
headerSize :: Word16
headerSize = 8

-- | Constant representing the Wayland null, which is just 0.
waylandNull :: Word32
waylandNull = 0

-- | `Get` parser for a Wayland header.
getHeader :: Get (WlUInt, Word16, Word16)
getHeader = (,,) . WlUInt <$> getWord32le <*> getWord16le <*> getWord16le

-- | `Put` a Wayland header. Reverses `getHeader`.
putHeader :: (RawObjectID, Word16, Word16) -> Put
putHeader (WlUInt oid, opcode, size) = putWord32le oid >> putWord16le opcode >> putWord16le size

{- | Create a wayland message. It takes an objectID, operation code and a message body.
The header is derived automatically.
-}
encodeMessage :: RawObjectID -> Word16 -> BS.ByteString -> BS.ByteString
encodeMessage oid opcode body = BSL.toStrict . runPut $ do
  putHeader (oid, opcode, headerSize + fromIntegral (BS.length body))
  putByteString body

-- | Why `decodeMessage` did not return a message.
data DecodeError
  = -- | Not all of the message has arrived yet. On a stream, read more and try again.
    Incomplete
  | -- | The header claims a size no message can have.
    InvalidSize InvalidSize
  deriving stock (Show, Eq)

instance Exception DecodeError

-- | Length of a message body as a count of 32-bit words.
newtype BodyWords = BodyWords Word16 deriving newtype (Show, Eq, Ord)

-- | Why `parseBodyWords` did not return a `BodyWords`.
data InvalidSize
  = -- | The header claims fewer bytes than the header itself takes up.
    SizeTooSmall Word16
  | -- | The header claims a size that is not a multiple of 4, which no message has.
    SizeUnaligned Word16
  deriving stock (Show, Eq)

instance Exception InvalidSize

-- | Parse the size of a body.
parseBodyWords :: Word16 -> Either InvalidSize BodyWords
parseBodyWords size
  | size < headerSize = Left (SizeTooSmall size)
  | size `mod` 4 /= 0 = Left (SizeUnaligned size)
  | otherwise = Right (BodyWords ((size - headerSize) `div` 4))

-- | Size of the body in bytes.
bodySize :: BodyWords -> Int
bodySize (BodyWords n) = 4 * fromIntegral n

-- | Parse the first message out of a `ByteString`, returning its object, opcode and body, and the bytes after it.
decodeMessage :: BS.ByteString -> Either DecodeError (RawObjectID, Word16, BS.ByteString, BS.ByteString)
decodeMessage s = case runGetOrFail getHeader (BS.fromStrict s) of
  Left _ -> Left Incomplete
  Right (rest', _, (oid, opcode, rawSize)) -> do
    bodyLength <- bimap InvalidSize bodySize (parseBodyWords rawSize)
    let (body, rest) = BS.splitAt bodyLength (BS.toStrict rest')
    if BS.length body < bodyLength then Left Incomplete else Right (oid, opcode, body, rest)
