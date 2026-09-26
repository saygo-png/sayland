{-# LANGUAGE RequiredTypeArguments #-}

-- | Description : Non protocol specific object management and communication.
module Sayland.Object (getClientEnv, newObjectId, newObject, runNewObjReq, sendMessage', interfaceFromName, getInterface, getInterface') where

import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.Data (cast)
import Data.Map qualified as Map
import Debug.Trace (traceIO)
import Network.Socket.ByteString (sendManyWithFds)
import Network.Socket.ByteString.Lazy (sendAll)
import Relude
import Relude.Extra (dup)
import Sayland.Core
import Sayland.Trace
import Sayland.Wire
import System.Console.ANSI (Color (..), ColorIntensity (..))

-- | Increases the counter by 1 and returns it's new value.
newObjectId :: Wayland p RawObjectID
newObjectId = do
  ClientEnv env <- ask
  liftIO $ atomicModifyIORef' env.counter $ dup . (+) 1

-- | Insert the given interface to the objects map with provided id as key.
newObject :: (Interface i p) => TObjectID i -> i -> Wayland p i
newObject (TObjectID intId) int = do
  objs <- (.objects) <$> getClientEnv
  _ <- atomicModifyIORef' objs $ dup . Map.insert intId (Interface int)
  pure int

-- | Insert the given interface to the objects map with provided id as key.
register :: (Object i) => TObjectID i -> (TObjectID i -> IO i) -> Wayland p i
register oid mk = do
  env <- getClientEnv
  obj <- liftIO (mk oid)
  _ <- atomicModifyIORef' objs $ dup . Map.insert intId (Interface int)
  -- collided <- atomicModifyIORef' env.objects $ \m ->
  --   case Map.insertLookupWithKey (\_ new _ -> new) (raw oid) (SomeObject obj) m of
  --     (old, m') -> (m', isJust old)
  -- when collided . throwIO $ Violation (raw oid) 0 "object id already in use"
  pure obj

-- | like `runRequest` but meant for use with creation requests. Returns the created interface.
runNewObjReq :: forall a b. (Interface a, Typeable b) => a -> (TObjectID b -> Request a) -> Wayland Client b
runNewObjReq i mkReq = do
  newId <- TObjectID <$> newObjectId
  runRequest i $ mkReq newId
  getInterface newId >>= \case
    Just child -> pure child
    Nothing ->
      error "runNewObjReq: runRequest did not register the expected object (library bug)"

-- | Send a message over the wire.
sendMessage' :: (Message m) => m -> TObjectID i -> Wayland p ()
sendMessage' e (TObjectID o) = do
  colorize <- liftIO getColorize
  liftIO (traceIO $ colorize Vivid Yellow $ ("    -> " <>) $ showEvent o e)
  socket' <- (.socket) <$> getClientEnv
  let (fds, body) = runPutM (execStateT (putEvent e) [])
      msg = mkMessage o (getOpcode e) body
  liftIO $ case reverse fds of
    [] -> sendAll socket' msg
    fds' -> sendManyWithFds socket' [BS.toStrict msg] fds'

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
getInterface' :: forall i p. (Typeable i) => RawObjectID -> Wayland p (Maybe i)
getInterface' objectID = do
  env <- getClientEnv
  (proxyInterface <=< Map.lookup objectID) <$> readIORef env.objects

-- | Cast provided interface into proxied type.
-- proxyInterface :: forall i p. (Typeable i) => Interface p -> Maybe i
-- proxyInterface (Interface i) = cast i
