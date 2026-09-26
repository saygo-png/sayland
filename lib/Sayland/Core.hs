{-# LANGUAGE TypeFamilyDependencies #-}

-- | Description : Core of the library that most other modules depend on.
module Sayland.Core (
  Wayland,
  Perspective (..),
  GlobalName (..),
  Object (..),
  TObjectID,
  raw,
  Global (..),
  ClientEnvironment (..),
  ServerEnvironment (..),
  KnownPerspective (..),
  EventHandler (..),
  Interface (..),
  SomeObject (..),
  Message (..),
  WaylandEnv (..),
  InterfaceEntry (..),
  ProtocolTable,
) where

import Control.Concurrent.STM (TQueue)
import Data.Binary
import Data.Data (typeOf)
import GHC.Records (HasField)
import Network.Socket (Socket)
import Relude hiding (ByteString, get, put)
import Sayland.Core.Internal (TObjectID, raw)
import Sayland.Wire
import System.Posix (Fd)

-- | The Wayland monad. Allows easy access to the Wayland environment state without threading repetitive arguments.
type Wayland p = ReaderT (WaylandEnv p) IO

{- | Type representing global names which are numbers.
Created in order to prevent mixups between object ids and global names.
-}
newtype GlobalName = GlobalName WlUInt
  deriving newtype (Show, Eq, Ord, Num)

class (Object i) => Global i where
  global :: (Coercible (TObjectID i) i) => TObjectID i -> IO i
  global = pure . coerce

{- | Sum type representing a perspective.
Used to make things reusable for clients and servers (compositors).
Should not be used on the term level.
-}
data Perspective = Client | Server -- TODO: Use TypeData here to remove term level constructors.

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
  -- | Effect of a request on local state. Runs on the sender and the receiver.
  onRequest :: i -> Request i -> Wayland p ()
  onRequest _ _ = pass

  -- | Effect of an event on local state. Runs on the sender and the receiver.
  onEvent :: i -> Event i -> Wayland p ()
  onEvent _ _ = pass

-- | Resolves which handler a message reaches, given the perspective it arrives from.
class KnownPerspective (p :: Perspective) where
  -- Route an incoming message to 'onEvent' on a client, 'onRequest' on a server.
  applyIncoming :: (Object i) => i -> Incoming p i -> Wayland p ()

  -- Route an outgoing message to 'onRequest' on a client, 'onEvent' on a server.
  applyOutgoing :: (Object i) => i -> Outgoing p i -> Wayland p ()

instance KnownPerspective Client where
  applyIncoming = onEvent
  applyOutgoing = onRequest

instance KnownPerspective Server where
  applyIncoming = onRequest
  applyOutgoing = onEvent

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
  getMessage :: Word16 -> WireGet m
  putMessage :: m -> WirePut ()
  getOpcode :: m -> Word16
  showMessage :: RawObjectID -> m -> String

-- | Everything needed to advertise and construct one interface.
data InterfaceEntry = InterfaceEntry
  { version :: Word32
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
  , interfaceTable :: IORef (Map WlString InterfaceEntry)
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
  , globals :: IORef (Map WlString GlobalName)
  -- ^ `Map` of globals advertised to the server indexable by interface name.
  , interfaceTable :: IORef (Map WlString InterfaceEntry)
  -- ^ Table of interfaces supported by the client. This allows for adding in custom protocols.
  , eventHandlers :: IORef [EventHandler p]
  -- ^ Custom event handlers that run on received events.
  , fdQueue :: TQueue Fd
  -- ^ Stores file descriptors.
  }
  deriving stock (Eq)

-- | Filler "implementation" for unimplemented methods.
unimplementedFor :: (Typeable a) => Text -> a -> b -> Wayland p ()
unimplementedFor meth x _ =
  error $ "sayland: " <> meth <> " is not implemented for " <> show (typeOf x)

-- vim: foldmethod=marker
