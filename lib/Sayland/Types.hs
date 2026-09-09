{-# LANGUAGE TypeFamilyDependencies #-}

module Sayland.Types (module Sayland.Types) where

import Control.Concurrent.STM (TQueue)
import Data.Bimap qualified as BM
import Data.Binary
import Data.Data (typeOf)
import GHC.Records (HasField)
import Network.Socket (Socket)
import Relude hiding (ByteString, get, put)
import Sayland.Wire.Types
import System.Posix (Fd)

-- | The Wayland monad. Allows easy access to the Wayland environment state without threading repetitive arguments.
type Wayland p = ReaderT (WaylandEnv p) IO

type ObjectID = WlUInt

newtype GlobalName = GlobalName WlUInt
  deriving newtype (Show, Eq, Ord, Num)

type TObjectID :: forall k. k -> Type

type role TObjectID phantom

newtype TObjectID a = TObjectID ObjectID deriving newtype (Show, Eq, Ord, Num)

instance WireFormat (TObjectID a) where
  wireGet = TObjectID <$> wireGet
  wirePut (TObjectID o) = wirePut o

-- a rectangle, described in pixels
data Rectangle = Rectangle
  { position :: (Int32, Int32)
  , size :: (Int32, Int32)
  }
  deriving stock (Eq, Ord)

-- | Class that allows to create an interface in IO with a wlid.
class NewInterface a where
  newInterface :: (MonadIO m) => TObjectID a -> m a

-- | Perspective of the current Wayland Environment
data Perspective = Client | Server

type role EventHandler nominal

-- | EventHandlers, called whenever an event is received
data EventHandler p where
  EventHandler :: (Typeable e, WaylandEvent e) => (ObjectID -> e -> Wayland p ()) -> EventHandler p

-- | Wayland Environment
type role WaylandEnv nominal

type ClientID = Int

data WaylandEnv (p :: Perspective) where
  ClientEnv :: ClientEnvironment Client -> WaylandEnv Client
  ClientServerEnv :: ServerEnvironment -> ClientEnvironment Server -> ClientID -> WaylandEnv Server

data ServerEnvironment = ServerEnvironment
  { socket :: Socket
  -- ^ global server socket
  , socketPath :: FilePath
  , clients :: TVar (Map ClientID (ClientEnvironment Server))
  -- ^ currently connected clients
  , clientSerial :: TVar ClientID
  -- ^ client counter, for identifying individual clients.
  , interfaceTable :: IORef (Map WlString (ObjectID -> IO (Interface Server)))
  -- ^ interfaces supported by the server
  , versionTable :: IORef (Map WlString WlUInt)
  -- ^ versions of interfaces
  , eventHandlers :: IORef [EventHandler Server]
  -- ^ server-side event handlers
  }

type role ClientEnvironment nominal

data ClientEnvironment (p :: Perspective) = ClientEnvironment
  { socket :: Socket
  , counter :: IORef ObjectID
  , objects :: IORef (Map ObjectID (Interface p))
  , globals :: IORef (BM.Bimap {-interface name-} WlString GlobalName)
  , interfaceTable :: IORef (Map WlString (ObjectID -> IO (Interface p)))
  , versionTable :: IORef (Map WlString WlUInt)
  , eventHandlers :: IORef [EventHandler p]
  , fdQueue :: TQueue Fd
  }
  deriving stock (Eq)

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

-- vim: foldmethod=marker
