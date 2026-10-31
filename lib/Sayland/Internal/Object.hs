{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Description : Internals of Sayland.Object.
module Sayland.Internal.Object (module Sayland.Internal.Object) where

import Control.Exception (Exception)
import Control.Monad
import Control.Monad.Reader (ask, liftIO, runReaderT)
import Data.Data
import Data.Map qualified as Map
import Sayland.Internal.Core
import Sayland.Internal.Prelude
import Sayland.Wire

-- | Takes the next `ObjectID` from the counter. Prefer `newObject`, or `withNewObjectID` to send the message yourself.
-- The peer must get new IDs in the order they were allocated. An ID made with this alone does not ensure that.
newObjectID :: (IDRange p) => Wayland p (ObjectID p)
newObjectID = withNewObjectID pure

-- | Error saying: Sayland ran out of numbers for `objectID`'s.
-- Will be thrown when there are IDs that were used, but got freed as we don't implement object ID reuse.
data ObjectIDsExhausted = ObjectIDsExhausted
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Allocate an `ObjectID` and send the message creating the object with it, Done as one step using a lock.
-- libwayland ends the connection of a peer whose new IDs arrive out of order, so allocating and sending must not interleave.
--
-- If the message is rejected before it is sent (an t'InvalidMessage'), the ID is given back. A gap would make the next ID out of order.
withNewObjectID :: (IDRange p) => (ObjectID p -> Wayland p a) -> Wayland p a
withNewObjectID send = do
  env <- getConnectionEnv
  waylandEnv <- ask
  liftIO . modifyMVar env.counter $ \current -> do
    oid <- maybe (throwIO ObjectIDsExhausted) pure current
    result <- runReaderT (send oid) waylandEnv
    pure (succObjectID oid, result)

-- | Create an object and tell the peer about it.
-- Used by users to create objects. Should not be used in handler implementations.
--
-- The object is looked up after its handler registered it and before the message goes out.
-- The peer can only answer (and destroy the object, like a @wl_callback@) once the message reached it, so the object must still be there.
newObject ::
  (Object parent, Object child, KnownPerspective p, IDRange p) =>
  parent -> (TObjectID child -> Outgoing p parent) -> Wayland p child
newObject parent mkMsg = withNewObjectID $ \i -> do
  let oid = TObjectID (toAnyObjectID i)
  outbox <- emitMsg parent (mkMsg oid)
  child <-
    getInterface oid
      >>= maybe (error $ "sayland bug: handler did not register object " <> show (toObjectID oid)) pure
  child <$ sendOutbox outbox

-- | Apply a handler associated with a message and then send it. If the message is malformed
-- i.e the handler throws t`ProtocolError`, this will throw t`InvalidMessage` and the message will NOT be sent.
-- If necessary raw messages without state changes can be sent using the internal `sendMessage`
sendMsg :: (KnownPerspective p, Object i) => i -> Outgoing p i -> Wayland p ()
sendMsg i m = emitMsg i m >>= sendOutbox

-- | Apply the handler of a message this side sends and return the resulting t`Outbox`
-- This function throws t`InvalidMessage`s if a handler throws t`ProtocolError`.
emitMsg :: (KnownPerspective p, Object i) => i -> Outgoing p i -> Wayland p Outbox
emitMsg i m = applyOutgoing i m `catchW` \(e :: ProtocolError) -> throwIO $ InvalidMessage e

-- Alias of `applyIncoming` for name consistency.
receiveMsg :: (Object i, KnownPerspective p) => i -> Incoming p i -> Wayland p Outbox
receiveMsg = applyIncoming

-- | Get an Interface using its id.
getInterface :: (Typeable i) => TObjectID i -> Wayland p (Maybe i)
getInterface (TObjectID oid) = do
  env <- getConnectionEnv
  (proxyInterface <=< Map.lookup oid) <$> readTVarIO env.objects

-- | `getInterface` inside a transaction, given the connection the object belongs to.
getInterfaceSTM :: (Typeable i) => ConnectionEnv p -> TObjectID i -> STM (Maybe i)
getInterfaceSTM env (TObjectID oid) = (proxyInterface <=< Map.lookup oid) <$> readTVar env.objects

-- | Cast provided interface into proxied type.
proxyInterface :: forall i. (Typeable i) => SomeObject -> Maybe i
proxyInterface (SomeObject o) = cast o
