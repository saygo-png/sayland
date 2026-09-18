-- | Description : Decoding and encoding wire protocol values.
module Sayland.Wire (WlInt (..), WlUInt (..), WlFixed (..), WlString (..), WlArray (..), WlFd (..), WlNewId (..), WireGet, WirePut, wireGet, wirePut, WireFormat, headerSize, waylandNull, getHeader, mkMessage) where

import Data.Binary (Get)
import Data.Binary.Get (getByteString, getInt32le, getWord16le, getWord32le, skip)
import Data.Binary.Put (PutM, putByteString, putInt32le, putLazyByteString, putWord16le, putWord32le, runPut)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Relude hiding (ByteString)
import Relude.Monad qualified as State
import System.Posix (Fd)

-- | WireGet monad, appends file descriptors to the Get monad.
type WireGet = StateT [Fd] Get

-- | WirePut monad, appends file descriptors to the PutM monad.
type WirePut = StateT [Fd] PutM

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

{- | Convenience function for formatting a Wayland message.
It takes an objectID, operation code and a message body.
The header is generated based on this, the size is derived automatically.
-}
mkMessage :: WlUInt -> Word16 -> BSL.ByteString -> BSL.ByteString
mkMessage objectID opcode messageBody =
  runPut $ do
    putWord32le $ coerce objectID
    putWord16le opcode
    putWord16le $ 8 + fromIntegral (BSL.length messageBody)
    putLazyByteString messageBody
