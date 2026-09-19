{-# LANGUAGE TypeFamilyDependencies #-}

-- | Description : Core of the library that most other modules depend on.
module Sayland.Core (module Sayland.Core) where

import Control.Concurrent.STM (TQueue)
import Data.Bimap qualified as BM
import Data.Binary
import Data.Data (typeOf)
import GHC.Records (HasField)
import Network.Socket (Socket)
import Relude hiding (ByteString, get, put)
import Sayland.Wire
import System.Posix (Fd)

-- | The Wayland monad. Allows easy access to the Wayland environment state without threading repetitive arguments.
type Wayland p = ReaderT (WaylandEnv p) IO

{- | Type representing global names which are numbers.
Created in order to prevent mixups between object ids and global names.
-}
newtype GlobalName = GlobalName WlUInt
  deriving newtype (Show, Eq, Ord, Num)

type TObjectID :: forall k. k -> Type

type role TObjectID phantom

-- | Type representing an `objectID` of a certain interface.
newtype TObjectID a = TObjectID ObjectID deriving newtype (Show, Eq, Ord, Num)

instance WireFormat (TObjectID a) where
  wireGet = TObjectID <$> wireGet
  wirePut (TObjectID o) = wirePut o

-- | Class that allows to create an interface in IO with a wlid.
class NewInterface a where
  newInterface :: (MonadIO m) => TObjectID a -> m a

-- | Perspective of the current Wayland Environment.
data Perspective = Client | Server

type role EventHandler nominal

-- | EventHandlers, called whenever an event is received.
data EventHandler p where
  EventHandler :: (Typeable e, WaylandEvent e) => (ObjectID -> e -> Wayland p ()) -> EventHandler p

-- | Number representing a Wayland Client.
type ClientID = Int

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
  , interfaceTable :: IORef (Map WlString (InterfaceEntry Server))
  -- ^ Table of interfaces supported by the server. This allows for adding in custom protocols.
  , eventHandlers :: IORef [EventHandler Server]
  -- ^ Server-side event handlers.
  }

type role ClientEnvironment nominal

-- | State required for a Wayland client.
data ClientEnvironment (p :: Perspective) = ClientEnvironment
  { socket :: Socket
  -- ^ Socket the client connects to.
  , counter :: IORef ObjectID
  -- ^ Mutable counter used to derive `objectID`s with increasing values.
  , objects :: IORef (Map ObjectID (Interface p))
  -- ^ Mutable `Map` of `objectID`s to interfaces they represent.
  , globals :: IORef (BM.Bimap {-interface name-} WlString GlobalName)
  -- ^ `Bimap` of globals advertised to the server stored as an interface name and `GlobalName`.
  , interfaceTable :: IORef (Map WlString (InterfaceEntry p))
  -- ^ Table of interfaces supported by the client. This allows for adding in custom protocols.
  , eventHandlers :: IORef [EventHandler p]
  -- ^ Custom event handlers that run on received events.
  , fdQueue :: TQueue Fd
  -- ^ Stores file descriptors.
  }
  deriving stock (Eq)

{- | Class defining an Interface as a collection of events and requests which has an `ObjectID`, version and name.
This does not include implementations of events and requests which are supplied by `Interface'`.
-}
class
  ( WaylandEvent (Event a)
  , WaylandEvent (Request a)
  , HasField "wlid" a (TObjectID a)
  , Typeable a
  ) =>
  IsInterface a
  where
  type Event a = r | r -> a
  type Request a = r | r -> a
  getInterfaceVersion :: Proxy a -> WlUInt
  getInterfaceName :: Proxy a -> WlString

class (IsInterface a) => Interface' a (p :: Perspective) where
  runEvent :: a -> Event a -> Wayland p ()
  runEvent = unimplementedFor "runEvent"
  runRequest :: a -> Request a -> Wayland p ()
  runRequest = unimplementedFor "runRequest"

-- | Filler "implementation" for unimplemented methods.
unimplementedFor :: (Typeable a) => Text -> a -> b -> Wayland p ()
unimplementedFor meth x _ =
  error $ "sayland: " <> meth <> " is not implemented for " <> show (typeOf x)

type role Interface nominal

data Interface (p :: Perspective) where
  Interface :: (Interface' i p, Typeable i) => i -> Interface p

class (Typeable e) => WaylandEvent e where
  getEvent :: Word16 -> WireGet e
  putEvent :: e -> WirePut ()
  getOpcode :: e -> Word16
  showEvent :: ObjectID -> e -> String

type role InterfaceEntry nominal

-- | Everything needed to advertise and construct one interface.
data InterfaceEntry (p :: Perspective) = InterfaceEntry
  { version :: WlUInt
  , construct :: ObjectID -> IO (Interface p)
  }

type ProtocolTable (p :: Perspective) = [(WlString, InterfaceEntry p)]

-- vim: foldmethod=marker
