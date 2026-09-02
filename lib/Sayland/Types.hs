{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Sayland.Types (module Sayland.Types) where

import Control.Concurrent.STM (TQueue)
import Control.Lens (Lens')
import Data.Bimap qualified as BM
import Data.Binary
import Data.ByteString qualified as BS
import Network.Socket (Socket)
import Relude hiding (ByteString, get, put)
import System.Posix (Fd)

type ObjectID = Word32

type TObjectID :: forall k. k -> Type

type role TObjectID phantom

newtype TObjectID a = TObjectID ObjectID deriving newtype (Show, Eq, Ord, Num)

type NewID = (BS.ByteString, Word32, ObjectID)

-- a rectangle, described in pixels
data Rectangle = Rectangle
  { position :: (Int, Int)
  , size :: (Int, Int)
  }
  deriving stock (Eq, Ord)

-- | HasWlid, a lens defined globally due to ID being a part of every wayland interface.
class HasWlid s a | s -> a where
  wlid :: Lens' s a

-- | A Default-like structure, but using IO
class DefaultIO a where
  defM :: (MonadIO m) => m a

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
  ClientEnv :: ClientEnvironment Client -> WaylandEnv 'Client
  ClientServerEnv :: ServerEnvironment -> ClientEnvironment Server -> ClientID -> WaylandEnv 'Server

data ServerEnvironment = ServerEnvironment
  { socket :: Socket
  -- ^ global server socket
  , socketPath :: FilePath
  , clients :: TVar (Map ClientID (ClientEnvironment Server))
  -- ^ currently connected clients
  , clientSerial :: TVar ClientID
  -- ^ client counter, for identifying individual clients.
  , interfaceTable :: IORef (Map String (IO (Interface Server)))
  -- ^ interfaces supported by the server
  , versionTable :: IORef (Map String Word32)
  -- ^ versions of interfaces
  , eventHandlers :: IORef [EventHandler Server]
  -- ^ server-side event handlers
  }

type role ClientEnvironment nominal

data ClientEnvironment (p :: Perspective) = ClientEnvironment
  { socket :: Socket
  , counter :: IORef Word32
  , objects :: IORef (Map Word32 (Interface p))
  , globals :: IORef (BM.Bimap {-string name-} BS.ByteString {-global name-} Word32)
  , interfaceTable :: IORef (Map String (IO (Interface p)))
  , versionTable :: IORef (Map String Word32)
  , eventHandlers :: IORef [EventHandler p]
  , fdQueue :: TQueue Fd
  }
  deriving stock (Eq)

class
  ( WaylandEvent (Event a)
  , WaylandEvent (Request a)
  , HasWlid a (TObjectID a)
  , Typeable a
  ) =>
  Interface' a (p :: Perspective)
  where
  type Event a
  type Request a
  runEvent :: a -> Event a -> Wayland p ()
  runRequest :: a -> Request a -> Wayland p ()

type role Interface nominal

data Interface (p :: Perspective) where
  Interface :: (Interface' i p, Typeable i) => i -> Interface p

class (Typeable e) => WaylandEvent e where
  getEvent :: Word16 -> AdditionalParserData -> IO (Get e)
  putEvent :: AdditionalParserData -> e -> Put
  getOpcode :: e -> Word16
  showEvent :: ObjectID -> e -> String

-- | Additional data passed to the TemplateHaskell-generated `getEvent`.
newtype AdditionalParserData = AdditionalParserData
  { fdqueue :: TQueue Fd
  }

-- | The Wayland monad. Allows easy access to the Wayland environment state without threading repetitive arguments.
type Wayland p = ReaderT (WaylandEnv p) IO

-- vim: foldmethod=marker
