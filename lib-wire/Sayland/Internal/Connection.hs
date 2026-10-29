-- | Description : Sending and receiving Wayland messages over a Unix socket.
--
-- On this branch the socket is one of the simulated "Sayland.Kernel", so dejafu can explore it.
module Sayland.Internal.Connection (module Sayland.Internal.Connection) where

import Data.Binary (Word16)
import Data.ByteString qualified as BS
import Data.String
import Sayland.Internal.Conc
import Sayland.Internal.Wire
import Sayland.Kernel
import System.FilePath
import System.Posix (Fd)
import Prelude

-- | Send one message to an object, along with any file descriptors the `WirePut` puts.
sendRaw :: Socket -> AnyObjectID -> Word16 -> WirePut () -> ConcIO ()
sendRaw sock oid opcode put = send sock msg fds
  where
    (body, fds) = runWirePut put
    msg = encodeMessage oid opcode body

-- | Receive some bytes, and any file descriptors sent along with them.
-- At most 8 bytes, like main's @recvMsg sock 8 4096@.
recvChunk :: Socket -> ConcIO (BS.ByteString, [Fd])
recvChunk sock = recv sock 8

-- | Get an absolute socket path based from @XDG_RUNTIME_DIR@.
getSocketPath :: Kernel -> ConcIO (Maybe String) -> ConcIO (Maybe FilePath)
getSocketPath kernel = liftA2 (liftA2 (</>)) $ getEnv kernel "XDG_RUNTIME_DIR"

-- | Find an already existing socket, if @WAYLAND_DISPLAY@ does not exist.
openSocketName :: Kernel -> ConcIO (Maybe String)
openSocketName kernel = findSocketName kernel (doesFileExist kernel)

-- | Find a not already existing and valid socket name.
-- Does NOT care about @WAYLAND_DISPLAY@
availableSocketName :: Kernel -> ConcIO (Maybe String)
availableSocketName kernel = scanRuntimeDir kernel (fmap not . doesFileExist kernel)

-- | Find a socket name by predicate.
-- Short circuits if @WAYLAND_DISPLAY@ exists, ignoring the predicate.
findSocketName :: Kernel -> (FilePath -> ConcIO Bool) -> ConcIO (Maybe String)
findSocketName kernel isAccepted = getEnv kernel "WAYLAND_DISPLAY" `orElse'` scanRuntimeDir kernel isAccepted
  where
    -- Run the second action only if the first yields Nothing.
    orElse' :: ConcIO (Maybe a) -> ConcIO (Maybe a) -> ConcIO (Maybe a)
    orElse' a b = a >>= maybe b (pure . Just)

-- | Find a socket name by predicate, scanning @XDG_RUNTIME_DIR@.
scanRuntimeDir :: Kernel -> (FilePath -> ConcIO Bool) -> ConcIO (Maybe String)
scanRuntimeDir kernel isAccepted =
  getEnv kernel "XDG_RUNTIME_DIR"
    >>= maybe (pure Nothing) (\dir -> firstMatch (isAccepted . (dir </>)) candidates)
  where
    candidates :: [String] = ["wayland-" <> fromString (show i) | i <- [0 .. 99 :: Int]]

    -- First element satisfying the predicate, stopping on the first match.
    firstMatch :: (a -> ConcIO Bool) -> [a] -> ConcIO (Maybe a)
    firstMatch p =
      foldr (\x rest -> p x >>= \ok -> if ok then pure (Just x) else rest) (pure Nothing)
