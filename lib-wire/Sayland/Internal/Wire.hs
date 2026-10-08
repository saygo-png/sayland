{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Description : Decoding and encoding wire protocol values.
module Sayland.Internal.Wire (module Sayland.Internal.Wire) where

import Control.Exception (Exception)
import Control.Monad.State.Strict (MonadTrans (lift), State, StateT)
import Control.Monad.State.Strict qualified as State
import Data.Bifunctor
import Data.Binary
import Data.Binary.Get (getInt32host, getWord16host, getWord32host, runGet)
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Functor
import Data.Int
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import GHC.TypeLits
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax qualified as TH
import System.Posix (Fd)
import Prelude

-- | WireGet monad. The state is the bytes and the file descriptors not taken yet.
-- It cannot fail by itself: a getter that can go wrong says so in its result.
type WireGet = State (BS.ByteString, [Fd])

-- | WirePut monad, appends file descriptors to the PutM monad.
type WirePut = StateT [Fd] PutM

-- | Run a `WirePut`, returning the bytes and the file descriptors it put, in the order they were put.
runWirePut :: WirePut () -> (BS.ByteString, [Fd])
runWirePut p = (BSL.toStrict bytes, reverse fds) -- `WlFd` conses, so the state is newest first.
  where
    (fds, bytes) = runPutM (State.execStateT p [])

-- | Run a `WireGet`, taking file descriptors from the front of the list.
-- Returns the bytes and the file descriptors it did not take.
runWireGet :: WireGet a -> [Fd] -> BS.ByteString -> (a, BS.ByteString, [Fd])
runWireGet g fds bytes = (result, restBytes, restFds)
  where
    (result, (restBytes, restFds)) = State.runState g (bytes, fds)

-- | Take the next @n@ bytes. `NotEnoughBytes`, taking nothing, if fewer are left.
getBytes :: Int -> WireGet (Either NotEnoughBytes BS.ByteString)
getBytes n = State.state $ \(bytes, fds) ->
  if BS.length bytes < n
    then (Left NotEnoughBytes, (bytes, fds))
    else let (taken, rest) = BS.splitAt n bytes in (Right taken, (rest, fds))

-- | Take @n@ bytes and decode them with a `Get` that reads exactly @n@ bytes, like `getWord32host`.
-- `runGet` cannot fail here, as `getBytes` already made sure there are @n@ bytes.
getHostWord :: Int -> Get a -> WireGet (Either NotEnoughBytes a)
getHostWord n getter = fmap (runGet getter . BS.fromStrict) <$> getBytes n

-- | Like `getWord32host`, but `NotEnoughBytes` instead of failing.
getWord32HostSafe :: WireGet (Either NotEnoughBytes Word32)
getWord32HostSafe = getHostWord 4 getWord32host

-- | Like `getInt32host`, but `NotEnoughBytes` instead of failing.
getInt32HostSafe :: WireGet (Either NotEnoughBytes Int32)
getInt32HostSafe = getHostWord 4 getInt32host

-- | Like `getWord16host`, but `NotEnoughBytes` instead of failing.
getWord16HostSafe :: WireGet (Either NotEnoughBytes Word16)
getWord16HostSafe = getHostWord 2 getWord16host

-- | Wayland @int@. 32-bit signed integer.
newtype WlInt = WlInt Int32 deriving newtype (Show, Eq, Ord, Num, Integral, Enum, Real)

-- | Wayland @uint@. 32-bit unsigned integer.
newtype WlUInt = WlUInt Word32 deriving newtype (Show, Eq, Ord, Num, Integral, Enum, Real)

-- | Wayland @fixed@. 24.8 bit signed fixed-point number.
newtype WlFixed = WlFixed Int32 deriving newtype (Show, Eq, Ord)

-- | The non null `Text` content of a `WlString`, encoded in UTF-8. Cannot contain NUL terminators.
newtype WlText = WlText Text deriving newtype (Show, Eq, Ord, Semigroup, Monoid)

-- | Fails on text containing NUL, which a wl_string can't carry.
wlText :: Text -> Either TextContainsNul WlText
wlText text = case T.findIndex (== '\0') text of
  Just index -> Left (TextContainsNul index)
  Nothing -> Right (WlText text)

-- | Any `Text` as a `WlText`, replacing each NUL with U+FFFD. For text only known at runtime.
wlTextLossy :: Text -> WlText
wlTextLossy = WlText . T.map (\ch -> if ch == '\0' then '\xFFFD' else ch)

-- | Wayland @string@.
-- UTF-8 encoded Text, prefixed with a 32-bit integer specifying its length (in bytes), followed by the string contents and a NUL terminator, padded to 32 bits with zero bytes.
-- The `Maybe` represents nullability. A `Nothing` value is a null string of len 0.
type WlString = Maybe WlText

-- | The contents of a `WlObjectID`. Unlike `WlUInt` but cannot be 0 (null).
newtype ObjectID = ObjectID WlUInt deriving newtype (Show, Eq, Ord)

-- | Make a valid `ObjectID`. Checked at compile time. Use with type application @mkObjectID \@5@
-- 0 (the null object) and anything that does not fit in 32 bits are type errors.
mkObjectID :: forall n. (KnownNat n, 1 <= n, n <= (2 ^ 32 - 1)) => ObjectID
mkObjectID = ObjectID . WlUInt . fromIntegral . natVal $ Proxy @n

-- | Get the next `ObjectID`.
-- `Nothing` if the next `ObjectID` would be higher than 4294967295 which is the largest number a @Word32@ can hold.
succObjectID :: ObjectID -> Maybe ObjectID
succObjectID (ObjectID n)
  | n >= WlUInt maxBound = Nothing
  | otherwise = Just $ ObjectID (n + 1)

-- | The number of an `ObjectID`.
fromObjectID :: ObjectID -> WlUInt
fromObjectID (ObjectID n) = n

-- | The wayland @object@. A null object is `Nothing`.
type WlObjectID = Maybe ObjectID

-- | `Nothing` for 0, the null object.
mkWlObjectID :: WlUInt -> WlObjectID
mkWlObjectID 0 = Nothing
mkWlObjectID n = Just (ObjectID n)

-- | Wayland @array@.
-- A blob of arbitrary data, prefixed with a 32-bit integer specifying its length (in bytes), then the verbatim contents of the array, padded to 32 bits with zero bytes.
newtype WlArray = WlArray BS.ByteString deriving newtype (Show, Eq, Ord)

-- | Wayland @fd@.
-- 0-bit value on the primary transport, but transfers a file descriptor to the other end using the ancillary data in the Unix domain socket message (msg_control).
-- This is the reason why we don't use a `Binary` instance for wire types, as we need additional state to house fds.
newtype WlFd = WlFd Fd deriving newtype (Show, Eq, Ord)

-- | Wayland @new_id@.
-- A 32-bit unspecified object ID. Preceded by a `WlString` specifying the interface name, and a `WlUInt` specifying the version.
data WlNewId = WlNewId WlString WlUInt WlUInt deriving stock (Show, Eq)

-- | Put a `WlUInt`.
putWlUInt :: WlUInt -> WirePut ()
putWlUInt (WlUInt w) = lift (putWord32host w)

-- | Get a `WlUInt`.
getWlUInt :: WireGet (Either NotEnoughBytes WlUInt)
getWlUInt = fmap WlUInt <$> getWord32HostSafe

-- | Put a `WlInt`.
putWlInt :: WlInt -> WirePut ()
putWlInt (WlInt w) = lift (putInt32host w)

-- | Get a `WlInt`.
getWlInt :: WireGet (Either NotEnoughBytes WlInt)
getWlInt = fmap WlInt <$> getInt32HostSafe

-- | Put a `WlFixed`.
putWlFixed :: WlFixed -> WirePut ()
putWlFixed (WlFixed w) = lift (putInt32host w)

-- | Get a `WlFixed`.
getWlFixed :: WireGet (Either NotEnoughBytes WlFixed)
getWlFixed = fmap WlFixed <$> getInt32HostSafe

-- | Put a `WlArray`, padded to 4 bytes.
putWlArray :: WlArray -> WirePut ()
putWlArray (WlArray bs) = lift $ do
  putWord32host (fromIntegral $ BS.length bs)
  putByteString bs
  putByteString (BS.replicate (padTo4 $ BS.length bs) 0)

-- | Get a `WlArray`, skipping its padding.
getWlArray :: WireGet (Either NotEnoughBytes WlArray)
getWlArray =
  getWlUInt >>= \case
    Left short -> pure (Left short)
    Right (WlUInt len) -> do
      contents <- getBytes (fromIntegral len)
      padding <- getBytes (padTo4 (fromIntegral len))
      pure (WlArray <$> contents <* padding)

-- | A `WlString` is a `WlArray` following some rules. Empty array is null. Otherwise the UTF-8 encoded text followed by a NUL terminator.
putWlString :: WlString -> WirePut ()
putWlString Nothing = putWlArray (WlArray "")
putWlString (Just (WlText text)) = putWlArray (WlArray (encodeUtf8 text <> "\0"))

-- | Get a `WlString`, or what is wrong with it.
getWlString :: WireGet (Either StringError WlString)
getWlString =
  getWlArray <&> \case
    Left short -> Left (StringTooShort short)
    Right (WlArray raw) -> case BS.unsnoc raw of
      Nothing -> Right Nothing
      Just (contents, 0) -> do
        text <- first (const NotUtf8) (decodeUtf8' contents)
        Just <$> first ContainsNul (wlText text)
      Just _ -> Left NotTerminated

-- | Put a `WlNewId`: its interface name, version and ID.
putWlNewId :: WlNewId -> WirePut ()
putWlNewId (WlNewId n v i) = putWlString n >> putWlUInt v >> putWlUInt i

-- | Get a `WlNewId`, or what is wrong with it.
getWlNewId :: WireGet (Either NewIdError WlNewId)
getWlNewId = do
  name <- getWlString
  version <- getWlUInt
  newId <- getWlUInt
  pure $ WlNewId <$> first NewIdName name <*> first NewIdTooShort version <*> first NewIdTooShort newId

-- | Put a `WlFd`. Its file descriptor is sent alongside the bytes, not in them.
putWlFd :: WlFd -> WirePut ()
putWlFd (WlFd f) = State.modify' (f :)

-- | Take the next received file descriptor. `Nothing` if none is left.
getWlFd :: WireGet (Maybe WlFd)
getWlFd =
  State.get >>= \case
    (_, []) -> pure Nothing
    (bytes, f : fs) -> Just (WlFd f) <$ State.put (bytes, fs)

-- | Like `getWlObjectID` but the null object is an error.
getObjectID :: WireGet (Either ObjectIDError ObjectID)
getObjectID =
  getWlObjectID <&> \case
    Left short -> Left (ObjectIDTooShort short)
    Right Nothing -> Left ObjectIDIsNul
    Right (Just oid) -> Right oid

-- | Put a non null `ObjectID`.
putObjectID :: ObjectID -> WirePut ()
putObjectID = putWlUInt . fromObjectID

-- | Get a `WlObjectID`. 0 is the null object, `Nothing`.
getWlObjectID :: WireGet (Either NotEnoughBytes WlObjectID)
getWlObjectID = fmap mkWlObjectID <$> getWlUInt

-- | Put a `WlObjectID`. `Nothing` is sent as 0.
putWlObjectID :: WlObjectID -> WirePut ()
putWlObjectID = putWlUInt . maybe 0 fromObjectID

-- | Number needed to round n up to the next multiple of 4.
-- Used to determine 0 byte padding for types such as `WlString` or `WlArray`.
padTo4 :: Int -> Int
padTo4 n = negate n `mod` 4

-- | The header size is always 8 in Wayland.
headerSize :: Word16
headerSize = 8

-- | `WireGet` parser for a Wayland header.
getHeader :: WireGet (Either NotEnoughBytes (WlObjectID, Word16, Word16))
getHeader = do
  oid <- getWlObjectID
  opcode <- getWord16HostSafe
  size <- getWord16HostSafe
  pure ((,,) <$> oid <*> opcode <*> size)

-- | `WirePut` a Wayland header. Reverses `getHeader`.
putHeader :: (ObjectID, Word16, Word16) -> WirePut ()
putHeader (oid, opcode, size) = putObjectID oid >> lift (putWord16host opcode >> putWord16host size)

-- | Create a wayland message. It takes an objectID, operation code and a message body.
-- The header is derived automatically.
encodeMessage :: ObjectID -> Word16 -> BS.ByteString -> BS.ByteString
encodeMessage oid opcode body = fst . runWirePut $ do
  putHeader (oid, opcode, headerSize + fromIntegral (BS.length body))
  lift (putByteString body)

-- | Length of a message body as a count of 32-bit words.
newtype BodyWords = BodyWords Word16 deriving newtype (Show, Eq, Ord)

-- | @wl_string@ in Wayland cannot contain NUL, because strings are NUL terminated.
newtype TextContainsNul = TextContainsNul Int deriving stock (Show, Eq)

instance Exception TextContainsNul

-- | Returned when the bytes run out before a `WireGet` is done.
data NotEnoughBytes = NotEnoughBytes deriving stock (Show, Eq)

instance Exception NotEnoughBytes

-- | Binary alignment errors.
data InvalidSize = SizeTooSmall Word16 | SizeUnaligned Word16 -- from parseBodyWords
  deriving stock (Show, Eq)

instance Exception InvalidSize

-- | What decoding a wl_string can get wrong.
data StringError
  = StringTooShort NotEnoughBytes -- wraps getWlArray's error, no copy
  | NotTerminated
  | NotUtf8
  | ContainsNul TextContainsNul -- wraps wlText's error, no copy
  deriving stock (Show, Eq)

instance Exception StringError

-- | What decoding a wl_new_id without a fixed interface can get wrong.
data NewIdError
  = NewIdTooShort NotEnoughBytes -- the version or ID ran out of bytes
  | NewIdName StringError -- wraps getWlString's error for the interface name, no copy
  deriving stock (Show, Eq)

instance Exception NewIdError

-- | What decoding a non null object can get wrong.
data ObjectIDError
  = ObjectIDTooShort NotEnoughBytes
  | -- | expected a non null object but a null one was received
    ObjectIDIsNul
  deriving stock (Show, Eq)

instance Exception ObjectIDError

-- | What splitting a stream into messages can get wrong. Used by `decodeMessage`
data DecodeError
  = Incomplete
  | InvalidSize InvalidSize
  deriving stock (Show, Eq)

instance Exception DecodeError

-- | `WlText` literals, checked while compiling. @[wl|wl_compositor|] :: WlText@
-- Text containing NUL is a compile error. Everything between the bars is the text, spaces included.
-- If you wish to create a WlString, wrap in `Just`. For a null WlString just use `Nothing`
wl :: QuasiQuoter
wl =
  QuasiQuoter
    { quoteExp = \s -> case wlText (T.pack s) of
        Left (TextContainsNul index) -> fail ("sayland: [wl|...|] contains NUL at index " <> show index)
        Right (WlText text) -> [|WlText $(TH.lift text)|]
    , quotePat = unsupported "patterns"
    , quoteType = unsupported "types"
    , quoteDec = unsupported "declarations"
    }
  where
    unsupported what _ = fail ("sayland: [wl|...|] only makes expressions, not " <> what)

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
decodeMessage :: BS.ByteString -> Either DecodeError (WlObjectID, Word16, BS.ByteString, BS.ByteString)
decodeMessage s = case runWireGet getHeader [] header of
  (Left NotEnoughBytes, _, _) -> Left Incomplete
  (Right (oid, opcode, rawSize), _, _) -> do
    bodyLength <- bimap InvalidSize bodySize (parseBodyWords rawSize)
    let (body, rest) = BS.splitAt bodyLength afterHeader
    if BS.length body < bodyLength then Left Incomplete else Right (oid, opcode, body, rest)
  where
    (header, afterHeader) = BS.splitAt (fromIntegral headerSize) s
