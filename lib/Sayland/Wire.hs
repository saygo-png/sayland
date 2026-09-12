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
  -- | Get a value from the wire
  wireGet :: WireGet a

  -- | Put a value from the wire
  wirePut :: a -> WirePut ()

newtype WlInt = WlInt Int32 deriving newtype (Show, Eq, Ord, Num, Integral, Enum, Real)

newtype WlUInt = WlUInt Word32 deriving newtype (Show, Eq, Ord, Num, Integral, Enum, Real)

newtype WlFixed = WlFixed Int32 deriving newtype (Show, Eq, Ord)

newtype WlString = WlString BS.ByteString deriving newtype (Show, Eq, Ord, IsString, Semigroup, Monoid)

newtype WlArray = WlArray BS.ByteString deriving newtype (Show, Eq, Ord)

newtype WlFd = WlFd Fd deriving newtype (Show, Eq, Ord)

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
