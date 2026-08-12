{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
module Saywayland.Types where

import Control.Lens (Lens')
import Data.Bimap qualified as BM
import Data.Binary
import Data.Binary.Put
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Data (cast)
import Data.Map qualified as Map
import Debug.Trace (traceIO)
import Network.Socket (Socket)
import Network.Socket.ByteString (sendManyWithFds)
import Network.Socket.ByteString.Lazy (sendAll)
import Relude hiding (ByteString, get, put)
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), hNowSupportsANSI, setSGRCode)
import System.Posix (Fd)
import Control.Concurrent.STM (TQueue)
import Data.Char (toUpper)

-- Constants {{{

-- | The header size is always 8 in Wayland.
headerSize :: Word16
headerSize = 8

-- | Constant representing the Wayland null, which is just 0.
waylandNull :: Word32
waylandNull = 0

-- | Constant representing the wl_display ID which is always 1 in Wayland.
wlDisplayID :: Word32
wlDisplayID = 1

-- | predefined empty AdditionalParserData
--nodata :: IO AdditionalParserData
--nodata = newIORef [] <&> AdditionalParserData

wlFormatter :: String -> String
wlFormatter [] = []
wlFormatter (x:xs) = toUpper x:xs

-- }}}

-- Types {{{

type ObjectID = Word32

type role TObjectID phantom
newtype TObjectID a = TObjectID ObjectID deriving newtype (Show, Eq, Ord, Num)

type NewID = (BS.ByteString, Word32, ObjectID)

-- a rectangle, described in pixels
data Rectangle = Rectangle
  { position  :: (Int, Int)
  , size      :: (Int, Int)
  }

-- | HasWlid, a lens defined globally due to ID being a part of every wayland interface.
class HasWlid s a | s -> a where
  wlid :: Lens' s a

-- | A Default-like structure, but using IO
class DefaultIO a where
  defM :: MonadIO m => m a

-- | Perspective of the current Wayland Environment
data Perspective = Client | Server

type role EventHandler nominal

-- | EventHandlers, called whenever an event is received
data EventHandler p where
  EventHandler :: (Typeable e, WaylandEvent e) => (ObjectID -> e -> Wayland p ()) -> EventHandler p

-- | Wayland Environment
type role WaylandEnv nominal

data WaylandEnv (p :: Perspective) where
  ClientEnv :: ClientEnvironment Client -> WaylandEnv 'Client
  ClientServerEnv :: ServerEnvironment -> ClientEnvironment Server -> WaylandEnv 'Server

data ServerEnvironment = ServerEnvironment
  { 
  -- | global server socket
    socket        :: Socket
  , socketPath    :: FilePath
  -- | currently connected clients
  , clients       :: TVar (Map Int (ClientEnvironment Server))
  -- | client counter, for identifying individual clients.
  , clientSerial  :: TVar Int
  -- | interfaces supported by the server
  , interfaceTable:: IORef (Map String (IO (Interface Server)))
  -- | versions of interfaces
  , versionTable  :: IORef (Map String Word32)
  -- | server-side event handlers
  , eventHandlers :: IORef [EventHandler Server]
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
  } deriving stock Eq

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

-- }}}

-- Utils {{{

-- | function that increases the counter by 1 and returns it's new value
newObjectId :: Wayland p Word32
newObjectId = do
  ClientEnv env <- ask
  liftIO $ modifyIORef env.counter (+ 1)
  liftIO $ readIORef env.counter

-- | function that inserts the given interface to the objects map with provided id as key.
newObject :: (Interface' i p) => TObjectID i -> i -> Wayland p i
newObject (TObjectID intId) int =
  ask >>= \case
    ClientEnv env -> liftIO (modifyIORef env.objects (Map.insert intId $ Interface int)) $> int
    ClientServerEnv _ env -> liftIO (modifyIORef env.objects (Map.insert intId $ Interface int)) $> int

-- | function that removes interface behind the provided id from the object map.
dropObject :: TObjectID a -> Wayland p ()
dropObject (TObjectID i) =
  ask >>= \case
    ClientEnv env -> modifyIORef env.objects $ Map.delete i
    ClientServerEnv _ env -> modifyIORef env.objects $ Map.delete i

{- | Convenience function for sending a Wayland message.
See 'mkMessage'.
-}
sendMessageWithFds :: [Fd] -> Word32 -> Word16 -> BSL.ByteString -> Wayland p ()
sendMessageWithFds fds objectID opcode messageBody =
  ask >>= \case
    ClientEnv env -> liftIO $ sendManyWithFds env.socket msg fds
    ClientServerEnv _ env -> liftIO $ sendManyWithFds env.socket msg fds
  where
    msg = [BS.toStrict $ mkMessage objectID opcode messageBody]

sendMessage :: Word32 -> Word16 -> BSL.ByteString -> Wayland p ()
sendMessage objectID opcode messageBody =
  ask >>= \case
    ClientEnv env -> liftIO . sendAll env.socket $ msg
    ClientServerEnv _ env -> liftIO . sendAll env.socket $ msg
  where
    msg = mkMessage objectID opcode messageBody

{- | Convenience function for formatting events, before sending them.
Events are colored in magenta following the wayland.app colorscheme.
-}
sendMessage' :: (WaylandEvent e) => e -> TObjectID i -> Wayland p ()
sendMessage' e (TObjectID o) = do
  colorize <- liftIO getColorize
  liftIO (traceIO $ colorize Vivid Magenta $ showEvent o e)
  q <- ask <&> \case
    ClientEnv env -> env.fdQueue
    ClientServerEnv _ env -> env.fdQueue
  let dat = AdditionalParserData q
  sendMessage o (getOpcode e) $ runPut $ putEvent dat e

-- | sendMessageWithFds, but with sendMessage' aspect.
sendMessageWithFds' :: (WaylandEvent e) => e -> [Fd] -> TObjectID i -> Wayland p ()
sendMessageWithFds' e fd (TObjectID o) = do
  colorize <- liftIO getColorize
  liftIO (traceIO $ colorize Vivid Magenta $ showEvent o e)
  q <- ask <&> \case
    ClientEnv env -> env.fdQueue
    ClientServerEnv _ env -> env.fdQueue
  let dat = AdditionalParserData q
  sendMessageWithFds fd o (getOpcode e) $ runPut $ putEvent dat e

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

getColorize :: (IsString s, Semigroup s) => IO (ColorIntensity -> Color -> s -> s)
getColorize = do
  ansiSupport <- hNowSupportsANSI stdout
  pure
    $ if ansiSupport
      then \ci c t -> fromString (setSGRCode [SetColor Foreground ci c]) <> t <> fromString (setSGRCode [Reset])
      else const $ const id

-- | Get the ClientEnvironment behind the Wayland monad.
getClientEnv :: Wayland p (ClientEnvironment p)
getClientEnv = ask <&> \case
  ClientEnv env -> env
  ClientServerEnv _ env -> env
-- | helper function for getting an object from a global
interfaceFromName :: Word32 -> Wayland p (Maybe BS.ByteString)
interfaceFromName n = do
  env <- getClientEnv
  glob <- readIORef env.globals
  pure $ BM.lookupR n glob

-- | get an Interface using its id.
getInterface :: Typeable i => TObjectID i -> Wayland p (Maybe i)
getInterface (TObjectID objectID) = do
  env <- getClientEnv
  (proxyInterface <=< Map.lookup objectID) <$> readIORef env.objects

-- | get an interface by @TypeApplication
getInterface' :: forall i p. (Typeable i) => Word32 -> Wayland p (Maybe i)
getInterface' objectID = do
  env <- getClientEnv
  (proxyInterface <=< Map.lookup objectID) <$> readIORef env.objects

-- | Round a byte length up to the nearest 4-byte boundary.
roundLength :: Word32 -> Int64
roundLength l = (fromIntegral l + 3) .&. (-4)

-- | Cast provided interface into proxied type.
proxyInterface :: forall i p. (Typeable i) => Interface p -> Maybe i
proxyInterface (Interface i) = cast i

-- }}}

-- vim: foldmethod=marker
