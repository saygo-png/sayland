module Sayland.Utils (getClientEnv, headerSize, waylandNull, wlDisplayID, newObjectId, newObject, sendMessage', sendMessageWithFds', interfaceFromName, getInterface, getInterface') where

import Data.Bimap qualified as BM
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Data (cast)
import Data.Map qualified as Map
import Debug.Trace (traceIO)
import Network.Socket.ByteString (sendManyWithFds)
import Network.Socket.ByteString.Lazy (sendAll)
import Relude
import Relude.Extra (dup)
import Sayland.Internal.Utils
import Sayland.Types
import System.Console.ANSI (Color (..), ColorIntensity (..))
import System.Posix (Fd)

-- | The header size is always 8 in Wayland.
headerSize :: Word16
headerSize = 8

-- | Constant representing the Wayland null, which is just 0.
waylandNull :: Word32
waylandNull = 0

-- | Constant representing the wl_display ID which is always 1 in Wayland.
wlDisplayID :: Word32
wlDisplayID = 1

-- | Increase the counter by 1 and returns it's new value.
newObjectId :: Wayland p Word32
newObjectId = do
  ClientEnv env <- ask
  liftIO $ atomicModifyIORef' env.counter $ dup . (+) 1

-- | Insert the given interface to the objects map with provided id as key.
newObject :: (Interface' i p) => TObjectID i -> i -> Wayland p i
newObject (TObjectID intId) int = do
  objs <- (.objects) <$> getClientEnv
  _ <- atomicModifyIORef' objs $ dup . Map.insert intId (Interface int)
  pure int

{- | Convenience function for sending a Wayland message.
See 'sendMessage'.
-}

-- | Send a Wayland message using the wire protocol.
sendMessage' :: (WaylandEvent e) => e -> TObjectID i -> Wayland p ()
sendMessage' e (TObjectID o) = do
  colorize <- liftIO getColorize
  liftIO (traceIO $ colorize Vivid Yellow $ ("    -> " <>) $ showEvent o e)
  q <- (.fdQueue) <$> getClientEnv
  let dat = AdditionalParserData q
  sendMessage o (getOpcode e) $ runPut $ putEvent dat e
  where
    sendMessage objectID opcode messageBody = do
      socket' <- (.socket) <$> getClientEnv
      liftIO $ sendAll socket' (mkMessage objectID opcode messageBody)

-- | Like `sendMessage` but also sends file descriptors.
sendMessageWithFds' :: (WaylandEvent e) => e -> [Fd] -> TObjectID i -> Wayland p ()
sendMessageWithFds' e fd (TObjectID o) = do
  colorize <- liftIO getColorize
  liftIO (traceIO $ colorize Vivid Yellow $ ("    -> " <>) $ showEvent o e)
  q <- (.fdQueue) <$> getClientEnv
  let dat = AdditionalParserData q
  sendMessageWithFds fd o (getOpcode e) $ runPut $ putEvent dat e
  where
    sendMessageWithFds fds objectID opcode messageBody = do
      socket' <- (.socket) <$> getClientEnv
      liftIO $ sendManyWithFds socket' [BS.toStrict $ mkMessage objectID opcode messageBody] fds

{- | Convenience function for formatting a Wayland message.
It takes an objectID, operation code and a message body.
The header is generated based on this, the size is derived automatically.
-}
mkMessage :: Word32 -> Word16 -> BSL.ByteString -> BSL.ByteString
mkMessage objectID opcode messageBody =
  runPut $ do
    putWord32le objectID
    putWord16le opcode
    putWord16le $ 8 + fromIntegral (BSL.length messageBody)
    putLazyByteString messageBody

-- | Get the ClientEnvironment behind the Wayland monad.
getClientEnv :: Wayland p (ClientEnvironment p)
getClientEnv =
  ask <&> \case
    ClientEnv env -> env
    ClientServerEnv _ env _ -> env

-- | Helper function for getting an object from a global.
interfaceFromName :: Word32 -> Wayland p (Maybe BS.ByteString)
interfaceFromName n = do
  env <- getClientEnv
  glob <- readIORef env.globals
  pure $ BM.lookupR n glob

-- | Get an Interface using its id.
getInterface :: (Typeable i) => TObjectID i -> Wayland p (Maybe i)
getInterface (TObjectID objectID) = do
  env <- getClientEnv
  (proxyInterface <=< Map.lookup objectID) <$> readIORef env.objects

-- | Get an Interface by @TypeApplication
getInterface' :: forall i p. (Typeable i) => Word32 -> Wayland p (Maybe i)
getInterface' objectID = do
  env <- getClientEnv
  (proxyInterface <=< Map.lookup objectID) <$> readIORef env.objects

-- | Cast provided interface into proxied type.
proxyInterface :: forall i p. (Typeable i) => Interface p -> Maybe i
proxyInterface (Interface i) = cast i
