{-# LANGUAGE RequiredTypeArguments #-}

-- | Description : Utilities that do not depend on any protocol.
module Sayland.Utils (getClientEnv, headerSize, waylandNull, newObjectId, newObject, runNewObjReq, sendMessage', interfaceFromName, getInterface, getInterface') where

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
import Sayland.Wire.Types
import System.Console.ANSI (Color (..), ColorIntensity (..))

-- | The header size is always 8 in Wayland.
headerSize :: Word16
headerSize = 8

-- | Constant representing the Wayland null, which is just 0.
waylandNull :: Word32
waylandNull = 0

-- | Increases the counter by 1 and returns it's new value.
newObjectId :: Wayland p ObjectID
newObjectId = do
  ClientEnv env <- ask
  liftIO $ atomicModifyIORef' env.counter $ dup . (+) 1

-- | Insert the given interface to the objects map with provided id as key.
newObject :: (Interface' i p) => TObjectID i -> i -> Wayland p i
newObject (TObjectID intId) int = do
  objs <- (.objects) <$> getClientEnv
  _ <- atomicModifyIORef' objs $ dup . Map.insert intId (Interface int)
  pure int

-- | like `runRequest` but meant for use with creation requests. Returns the created interface.
runNewObjReq :: forall a b. (Interface' a Client, Typeable b) => a -> (TObjectID b -> Request a) -> Wayland Client b
runNewObjReq i mkReq = do
  newId <- TObjectID <$> newObjectId
  runRequest i $ mkReq newId
  getInterface newId >>= \case
    Just child -> pure child
    Nothing ->
      error "runNewObjReq: runRequest did not register the expected object (library bug)"

-- | Send a message over the wire.
sendMessage' :: (WaylandEvent e) => e -> TObjectID i -> Wayland p ()
sendMessage' e (TObjectID o) = do
  colorize <- liftIO getColorize
  liftIO (traceIO $ colorize Vivid Yellow $ ("    -> " <>) $ showEvent o e)
  socket' <- (.socket) <$> getClientEnv
  let (fds, body) = runPutM (execStateT (putEvent e) [])
      msg = mkMessage o (getOpcode e) body
  liftIO $ case reverse fds of
    [] -> sendAll socket' msg
    fds' -> sendManyWithFds socket' [BS.toStrict msg] fds'

{- | Convenience function for formatting a Wayland message.
It takes an objectID, operation code and a message body.
The header is generated based on this, the size is derived automatically.
-}
mkMessage :: ObjectID -> Word16 -> BSL.ByteString -> BSL.ByteString
mkMessage objectID opcode messageBody =
  runPut $ do
    putWord32le $ coerce objectID
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
interfaceFromName :: GlobalName -> Wayland p (Maybe WlString)
interfaceFromName n = do
  env <- getClientEnv
  globals <- readIORef env.globals
  pure $ BM.lookupR n globals

-- | Get an Interface using its id.
getInterface :: (Typeable i) => TObjectID i -> Wayland p (Maybe i)
getInterface (TObjectID objectID) = do
  env <- getClientEnv
  (proxyInterface <=< Map.lookup objectID) <$> readIORef env.objects

-- | Get an Interface by @TypeApplication
getInterface' :: forall i p. (Typeable i) => ObjectID -> Wayland p (Maybe i)
getInterface' objectID = do
  env <- getClientEnv
  (proxyInterface <=< Map.lookup objectID) <$> readIORef env.objects

-- | Cast provided interface into proxied type.
proxyInterface :: forall i p. (Typeable i) => Interface p -> Maybe i
proxyInterface (Interface i) = cast i
