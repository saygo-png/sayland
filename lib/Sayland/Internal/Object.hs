{-# OPTIONS_HADDOCK not-home #-}

-- | Description : Internals of Sayland.Object.
module Sayland.Internal.Object (module Sayland.Internal.Object) where

import Control.Monad
import Data.Data
import Data.Map qualified as Map
import Sayland.Internal.Core
import Sayland.Internal.Prelude
import Sayland.Wire

-- | Enter an object in this side's object map.
registerObject :: (Object i) => i -> Wayland p ()
registerObject obj = do
  env <- getClientEnv
  atomicModifyIORef' env.objects $ \m -> (Map.insert (toRawObjectID obj.wlid) (SomeObject obj) m, ())

-- | Increases the counter by 1 and returns it's new value.
newObjectID :: Wayland p RawObjectID
newObjectID = do
  -- TODO: There is an upper bound to object ids that this does not yet enforce.
  env <- getClientEnv
  atomicModifyIORef' env.counter $ \n -> (n + 1, n + 1)

{- | Create an object and tell the peer about it.
Used by users to create objects. Should not be used in handler implementations.
-}
newObject ::
  (Object parent, Object child, KnownPerspective p) =>
  parent -> (TObjectID child -> Outgoing p parent) -> Wayland p child
newObject parent mkMsg = do
  oid <- TObjectID <$> newObjectID
  sendMsg parent (mkMsg oid)
  getInterface oid
    >>= maybe (error $ "sayland bug: handler did not register object " <> show (toRawObjectID oid)) pure

{- | Send a message and apply a handler associated with it. The handler changes the state.
If necessary raw messages without state changes can be sent using the internal `sendMessage`
-}
sendMsg :: (KnownPerspective p, Object i) => i -> Outgoing p i -> Wayland p ()
sendMsg i m = applyOutgoing i m `catchW` \(e :: ProtocolError) -> throwIO $ InvalidMessage e

{- | Receive a message, applying state changes associated with it.
Alias of `applyIncoming` for name consistency.
-}
receiveMsg :: (Object i, KnownPerspective p) => i -> Incoming p i -> Wayland p ()
receiveMsg = applyIncoming

-- | Get an Interface using its id.
getInterface :: (Typeable i) => TObjectID i -> Wayland p (Maybe i)
getInterface (TObjectID objectID) = do
  env <- getClientEnv
  (proxyInterface <=< Map.lookup objectID) <$> readIORef env.objects

-- | Cast provided interface into proxied type.
proxyInterface :: forall i. (Typeable i) => SomeObject -> Maybe i
proxyInterface (SomeObject o) = cast o
