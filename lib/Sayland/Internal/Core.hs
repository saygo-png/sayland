{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Description : Internals of Sayland.Core.
module Sayland.Internal.Core (module Sayland.Internal.Core) where

import Control.Concurrent.STM
import Control.Exception hiding (throwIO)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Binary
import Data.Binary.Put (runPutM)
import Data.ByteString qualified as BS
import Data.Coerce
import Data.Kind
import Data.Map (Map)
import Data.Typeable
import Debug.Trace (traceIO)
import GHC.Records (HasField)
import Network.Socket (Socket)
import Network.Socket.ByteString (sendManyWithFds)
import Network.Socket.ByteString.Lazy (sendAll)
import Sayland.Internal.Prelude
import Sayland.Internal.Trace (getColorize)
import Sayland.Wire
import System.Console.ANSI
import System.Posix (Fd)

-- | Type representing an `objectID` of a certain object.
newtype TObjectID (a :: Type) = TObjectID RawObjectID deriving newtype (Show, Eq, Ord)

type role TObjectID phantom

-- | Internal class for converting shit to a rawObjectID.
class ToRawObjectID o where
  toRawObjectID :: o -> RawObjectID

instance ToRawObjectID (TObjectID a) where
  toRawObjectID = coerce

instance ToRawObjectID WlUInt where
  toRawObjectID = id

-- forces UndecidableInstances and OVERLAPPABLE x_x
instance {-# OVERLAPPABLE #-} (Interface o) => ToRawObjectID o where
  toRawObjectID o = toRawObjectID o.wlid

instance WireFormat (TObjectID a) where
  wireGet = TObjectID <$> wireGet
  wirePut (TObjectID o) = wirePut o

-- | The Wayland monad. Allows easy access to the Wayland environment state without threading repetitive arguments.
type Wayland p = ReaderT (WaylandEnv p) IO

{- | Type representing global names which are numbers.
Created in order to prevent mixups between object ids and global names.
-}
newtype GlobalName = GlobalName WlUInt
  deriving newtype (Show, Eq, Ord, Num)

class (Object i) => Global i where
  global :: TObjectID i -> IO i
  default global :: (Coercible (TObjectID i) i) => TObjectID i -> IO i
  global = pure . coerce

{- | Sum type representing a perspective.
Used to make things reusable for clients and servers (compositors).
-}
data Perspective = Client | Server
  deriving stock (Eq)

-- | Incoming message to Client/Server's object
type Incoming :: Perspective -> Type -> Type

-- | Incoming message to Client/Server's object
type Outgoing :: Perspective -> Type -> Type

-- | Defines the relation of incoming messages to a client and server
type family Incoming (p :: Perspective) (i :: Type) where
  Incoming Client i = Event i -- Clients receive events.
  Incoming Server i = Request i -- Servers receive requests.

-- | Defines the relation of outgoing messages to a client and server.
type family Outgoing p i where
  Outgoing Client i = Request i -- Clients send requests.
  Outgoing Server i = Event i -- Servers send events.

-- | An interface that can be used as an object. In other words, the implementation of an interface.
class (Interface i) => Object i where
  -- | Effect of a request on local state + sendMessage. Runs on the sender and the receiver.
  onRequest :: i -> Request i -> Wayland p ()

  -- | Effect of an event on local state + sendMessage. Runs on the sender and the receiver.
  onEvent :: i -> Event i -> Wayland p ()

-- | Resolves which handler a message reaches, given the perspective it arrives from.
class KnownPerspective (p :: Perspective) where
  -- | Route an incoming message to 'onEvent' on a client, 'onRequest' on a server.
  applyIncoming :: (Object i) => i -> Incoming p i -> Wayland p ()

  -- | Route an outgoing message to 'onRequest' on a client, 'onEvent' on a server.
  applyOutgoing :: (Object i) => i -> Outgoing p i -> Wayland p ()

  -- | Typeclass bs. Bring the 'Message' instance of the messages this side receives for @i@ into scope.
  withIncoming :: (Object i) => i -> ((Message (Incoming p i)) => Wayland p r) -> Wayland p r

instance KnownPerspective Client where
  applyIncoming = onEvent
  applyOutgoing = onRequest
  withIncoming _ k = k -- Incoming Client i = Event i, Message via Interface

instance KnownPerspective Server where
  applyIncoming = onRequest
  applyOutgoing = onEvent
  withIncoming _ k = k -- Incoming Server i = Request i, Message via Interface

type role EventHandler nominal

-- | EventHandlers, called whenever an event is received.
data EventHandler p where
  EventHandler :: (Typeable m, Message m) => (RawObjectID -> m -> Wayland p ()) -> EventHandler p

-- | Number representing a Wayland Client.
type ClientID = Int

{- | Class defining an Interface as a collection of events and requests which has a `TObjectID`, version and name.
This does not include implementations of events and requests which are supplied by `Interface`.
-}
class
  ( Message (Event a)
  , Message (Request a)
  , HasField "wlid" a (TObjectID a)
  , Typeable a
  ) =>
  Interface a
  where
  type Event a = r | r -> a
  type Request a = r | r -> a
  getInterfaceVersion :: Proxy a -> WlUInt
  getInterfaceName :: Proxy a -> WlString

-- | Existential type for holding different objects in one structure.
data SomeObject where
  SomeObject :: (Object i) => i -> SomeObject

class (Typeable m) => Message m where
  sender :: m -> Perspective
  getMessage :: Word16 -> WireGet m
  putMessage :: m -> WirePut ()
  getOpcode :: m -> Word16
  showMessage :: RawObjectID -> m -> String

-- | Everything needed to advertise and construct one interface.
data InterfaceEntry = InterfaceEntry
  { version :: WlUInt
  , construct :: RawObjectID -> IO SomeObject
  }

type ProtocolTable = [(WlString, InterfaceEntry)]

type role WaylandEnv nominal

-- | State required for either a Wayland server or Client.
data WaylandEnv (p :: Perspective) where
  ClientEnv :: ClientEnvironment Client -> WaylandEnv Client
  ClientServerEnv :: ServerEnvironment -> ClientEnvironment Server -> ClientID -> WaylandEnv Server

-- | State required for a Wayland server.
data ServerEnvironment = ServerEnvironment
  { socket :: Socket
  -- ^ global server socket
  , socketPath :: FilePath
  , clients :: TVar (Map ClientID (ClientEnvironment Server))
  -- ^ Currently connected clients.
  , clientSerial :: TVar ClientID
  -- ^ Client counter, for identifying individual clients.
  , interfaceTable :: Map WlString InterfaceEntry
  -- ^ Table of interfaces supported by the server. This allows for adding in custom protocols.
  , eventHandlers :: IORef [EventHandler Server]
  -- ^ Server-side event handlers.
  }

type role ClientEnvironment nominal

-- | State required for a Wayland client.
data ClientEnvironment (p :: Perspective) = ClientEnvironment
  { socket :: Socket
  -- ^ Socket the client connects to.
  , counter :: IORef RawObjectID
  -- ^ Mutable counter used to derive `objectID`s with increasing values.
  , objects :: IORef (Map RawObjectID SomeObject)
  -- ^ Mutable `Map` of `objectID`s to interfaces they represent.
  , globals :: IORef (Map GlobalName (WlString, WlUInt))
  -- ^ `Map` of global interfaces and their versions advertised by a compositor.
  , interfaceTable :: Map WlString InterfaceEntry
  -- ^ Table of interfaces supported by the client. This allows for adding in custom protocols.
  , eventHandlers :: IORef [EventHandler p]
  -- ^ Custom event handlers that run on received events.
  , fdQueue :: TQueue Fd
  -- ^ Stores file descriptors.
  }

{- | Error saying: The connection is over. The peer reported a violation with @wl_display.error@,
or sent something invalid. This represents a wire value so it only carries wire types. (no `TObjectID` or `ErrorCode`)
-}
data ProtocolError = ProtocolError
  { object :: RawObjectID
  , code :: WlUInt
  , message :: WlString
  }
  deriving stock (Show)

instance Exception ProtocolError where
  displayException e =
    "wayland protocol error on object " <> show e.object <> " (code " <> show e.code <> "): " <> show e.message

-- | Enums a protocol declares as error codes (@\<enum name="error"\>@).
class ErrorCode e where
  errorCode :: e -> WlUInt

{- | Error saying: A message failed validation before it was sent. Nothing reached the peer and
the connection is still usable.
-}
newtype InvalidMessage = InvalidMessage ProtocolError
  deriving stock (Show)

instance Exception InvalidMessage where
  displayException (InvalidMessage e) = "message not sent: " <> displayException e

-- | Error saying: The compositor does not advertise a global the client requires.
newtype MissingGlobal = MissingGlobal WlString
  deriving stock (Show)

instance Exception MissingGlobal where
  displayException (MissingGlobal iface) = "the compositor does not advertise the required global " <> show iface

-- | The connection could not be made or was lost.
data ConnectionError
  = NoSocket FilePath
  | Disconnected
  deriving stock (Show)
  deriving anyclass (Exception)

{- | The null object.
In the future this should be replaced by all nullable object types being `Maybe` and users passing in `Nothing`.
-}
nullObjectID :: TObjectID a
nullObjectID = TObjectID 0

{-# WARNING in "x-stub" stub "Handled by a stub: this message is not fully implemented." #-}

{- | Placeholder for a message which is not fully implemented yet.
Avoid using this function if possible. Instead implement an interface fully or ditch it.
Partial interfaces are worse than an unimplemented one because they have inconsistent behaviour.
Use this instead of just `pass` or only forwarding as it warns at compile time and logs at runtime.
-}
stub :: (Message m, HasField "wlid" i (TObjectID i)) => i -> m -> Wayland p ()
stub obj msg = liftIO . traceIO $ "sayland: unimplemented: " <> showMessage (toRawObjectID obj.wlid) msg

-- | Placeholder for a message which are not implemented and not planned to be due to being deprecated.
stubDeprecated :: (Message m, HasField "wlid" i (TObjectID i)) => i -> m -> Wayland p ()
stubDeprecated obj msg = liftIO . traceIO $ "sayland: unimplemented(deprecated): " <> showMessage (toRawObjectID obj.wlid) msg

{- | Like `sendMessage` but meant for use inside handlers.
It contains the logic determining if a message should be sent from the current perspective.
-}
forwardMessage :: (Message m, HasField "wlid" i (TObjectID i)) => i -> m -> Wayland p ()
forwardMessage i m = do
  me <-
    asks $ \case
      ClientEnv{} -> Client
      ClientServerEnv{} -> Server
  when (sender m == me) $ sendMessage m i.wlid

-- | 'catch' for the Wayland monad.
catchW :: (Exception e) => Wayland p a -> (e -> Wayland p a) -> Wayland p a
catchW act handler = do
  env <- ask
  liftIO $ runReaderT act env `catch` \e -> runReaderT (handler e) env

-- | Helper for throwing a protocolError
protocolError :: (MonadIO m, ToRawObjectID o, ErrorCode e) => o -> e -> WlString -> m a
protocolError o code msg = throwIO $ ProtocolError (toRawObjectID o) (errorCode code) msg

-- | Helper. The value in a 'Maybe', or the fallback action if there's none.
whenNothing :: (Applicative f) => Maybe a -> f a -> f a
whenNothing m fallback = maybe fallback pure m

-- | Get the ClientEnvironment behind the Wayland monad.
getClientEnv :: Wayland p (ClientEnvironment p)
getClientEnv =
  asks $ \case
    ClientEnv env -> env
    ClientServerEnv _ env _ -> env

-- | Run something only on the server side. Used for implementing interfaces.
onServer :: Wayland Server () -> Wayland p ()
onServer k =
  ask >>= \case
    ClientServerEnv{} -> k
    ClientEnv{} -> pass

-- | Run something only on the client side. Used for implementing interfaces.
onClient :: Wayland Client () -> Wayland p ()
onClient k =
  ask >>= \case
    ClientServerEnv{} -> pass
    ClientEnv{} -> k

-- | Send a raw message over the wire.
sendMessage :: (Message m) => m -> TObjectID i -> Wayland p ()
sendMessage e (TObjectID o) = do
  colorize <- liftIO getColorize
  liftIO (traceIO $ colorize Vivid Yellow $ ("    -> " <>) $ showMessage o e)
  socket' <- (.socket) <$> getClientEnv
  let (fds, body) = runPutM (execStateT (putMessage e) [])
      msg = encodeMessage o (getOpcode e) body
  liftIO $ case reverse fds of
    [] -> sendAll socket' msg
    fds' -> sendManyWithFds socket' [BS.toStrict msg] fds'
