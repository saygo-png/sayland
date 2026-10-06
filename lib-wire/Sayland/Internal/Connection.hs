-- | Description : Sending and receiving Wayland messages over a Unix socket.
module Sayland.Internal.Connection (module Sayland.Internal.Connection) where

import Data.Binary (Word16)
import Data.ByteString qualified as BS
import Data.String
import Foreign (Storable (peek, sizeOf), castPtr)
import Foreign.C
import Network.Socket
import Network.Socket.ByteString (recvMsg, sendAll, sendManyWithFds)
import Sayland.Internal.Wire
import System.Directory (doesFileExist)
import System.Environment.Blank (getEnv)
import System.FilePath
import System.Posix (Fd (Fd))
import Prelude

-- | Send one message to an object, along with any file descriptors the `WirePut` puts.
sendRaw :: Socket -> RawObjectID -> Word16 -> WirePut () -> IO ()
sendRaw sock oid opcode put = case fds of
  [] -> sendAll sock msg
  _ -> sendManyWithFds sock [msg] fds
  where
    (body, fds) = runWirePut put
    msg = encodeMessage oid opcode body

-- | Receive some bytes, and any file descriptors sent along with them.
recvChunk :: Socket -> IO (BS.ByteString, [Fd])
recvChunk sock = do
  (_, bytes, cmsgs, _flags) <- recvMsg sock 8 4096 mempty
  fds <- concat <$> traverse (decodeFds . cmsgData) (filter (\x -> cmsgId x == CmsgIdFds) cmsgs)
  pure (bytes, fds)

-- | Get a list of file descriptors from an ancillary data bytestring.
decodeFds :: BS.ByteString -> IO [Fd]
decodeFds bs = map Fd <$> go bs []
  where
    intSize = sizeOf (0 :: CInt)
    go b acc
      | BS.length b < intSize = pure $ reverse acc
      | otherwise = do
          let (x, rest) = BS.splitAt intSize b
          v <- BS.useAsCString x (peek . castPtr)
          go rest (v : acc)

-- | Get an absolute socket path based from @XDG_RUNTIME_DIR@.
getSocketPath :: IO (Maybe String) -> IO (Maybe FilePath)
getSocketPath = liftA2 (liftA2 (</>)) $ getEnv "XDG_RUNTIME_DIR"

-- | Find an already existing socket, if @WAYLAND_DISPLAY@ does not exist.
openSocketName :: IO (Maybe String)
openSocketName = findSocketName doesFileExist

-- | Find a not already existing and valid socket name.
-- Does NOT care about @WAYLAND_DISPLAY@
availableSocketName :: IO (Maybe String)
availableSocketName = scanRuntimeDir (fmap not . doesFileExist)

-- | Find a socket name by predicate.
-- Short circuits if @WAYLAND_DISPLAY@ exists, ignoring the predicate.
findSocketName :: (FilePath -> IO Bool) -> IO (Maybe String)
findSocketName isAccepted = getEnv "WAYLAND_DISPLAY" `orElse'` scanRuntimeDir isAccepted
  where
    -- Run the second action only if the first yields Nothing.
    orElse' :: IO (Maybe a) -> IO (Maybe a) -> IO (Maybe a)
    orElse' a b = a >>= maybe b (pure . Just)

-- | Find a socket name by predicate, scanning @XDG_RUNTIME_DIR@.
scanRuntimeDir :: (FilePath -> IO Bool) -> IO (Maybe String)
scanRuntimeDir isAccepted =
  getEnv "XDG_RUNTIME_DIR"
    >>= maybe (pure Nothing) (\dir -> firstMatch (isAccepted . (dir </>)) candidates)
  where
    candidates :: [String] = ["wayland-" <> fromString (show i) | i <- [0 .. 99 :: Int]]

    -- First element satisfying the predicate, stopping on the first match.
    firstMatch :: (a -> IO Bool) -> [a] -> IO (Maybe a)
    firstMatch p =
      foldr (\x rest -> p x >>= \ok -> if ok then pure (Just x) else rest) (pure Nothing)
