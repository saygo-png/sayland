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
import Data.Binary
import Data.ByteString qualified as BS
import Data.Coerce
import Data.Kind
import Data.Map (Map)
import Data.Typeable
import Debug.Trace (traceIO)
import GHC.Records (HasField)
import Network.Socket (Socket)
import Sayland.Internal.Prelude
import Sayland.Internal.Trace (getColorize)
import Sayland.Wire
import System.Console.ANSI
import System.Posix (Fd)

-- | Type representing an `objectID` of a certain object.
newtype TObjectID (a :: Type) = TObjectID ObjectID deriving newtype (Show, Eq, Ord)

type role TObjectID phantom

-- | Internal class for converting shit to a rawObjectID.
class ToObjectID o where
  toObjectID :: o -> ObjectID

instance ToObjectID (TObjectID a) where
  toObjectID = coerce

-- forces UndecidableInstances and OVERLAPPABLE x_x
instance {-# OVERLAPPABLE #-} (Interface o) => ToObjectID o where
  toObjectID o = toObjectID o.wlid

-- | The Wayland monad. Allows easy access to the Wayland environment state without threading repetitive arguments.
type Wayland p = ReaderT (WaylandEnv p) IO

-- | Type representing global names which are numbers.
-- Created in order to prevent mixups between object ids and global names.
newtype GlobalName = GlobalName WlUInt
  deriving newtype (Show, Eq, Ord, Num)

class (Object i) => Global i where
  global :: TObjectID i -> IO i
  default global :: (Coercible (TObjectID i) i) => TObjectID i -> IO i
  global = pure . coerce

-- | Sum type representing a perspective.
-- Used to make things reusable for clients and servers (compositors).
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
  EventHandler :: (Typeable m, Message m) => (ObjectID -> m -> Wayland p ()) -> EventHandler p

-- | Number representing a Wayland Client.
type ClientID = Int

-- | Class defining an Interface as a collection of events and requests which has a `TObjectID`, version and name.
-- This does not include implementations of events and requests which are supplied by `Interface`.
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
  getInterfaceName :: Proxy a -> WlText

-- | Existential type for holding different objects in one structure.
data SomeObject where
  SomeObject :: (Object i) => i -> SomeObject

class (Typeable m) => Message m where
  sender :: m -> Perspective

  -- | Get the message with this opcode.
  getMessage :: Word16 -> WireGet (Either MessageError m)

  putMessage :: m -> WirePut ()
  getOpcode :: m -> Word16
  showMessage :: ObjectID -> m -> String

-- | What decoding a message body can get wrong.
-- Wraps the errors of the wire getters and adds protocol based errors.
data MessageError
  = -- | The interface has no message with this opcode.
    UnknownOpcode Word16
  | -- | The args the protocol declares need more bytes than the body has.
    BodyTooShort NotEnoughBytes
  | -- | The body has bytes after the args the protocol declares.
    TrailingBytes
  | -- | The protocol declares an fd but none was received.
    MissingFd
  | -- | Wraps `getWlString`'s error.
    InvalidString StringError
  | -- | Wraps `getWlNewId`'s error.
    InvalidNewId NewIdError
  | -- | Wraps `getObjectID`'s error. Null where the protocol does not mark @allow-null@.
    InvalidObjectID ObjectIDError
  | -- | A string the protocol does not mark @allow-null@ was null.
    NullString
  | -- | Not a value of the enum that the protocol declares for the argument.
    UnknownEnumValue WlUInt
  deriving stock (Show, Eq)

instance Exception MessageError

-- | Decode a message body, which must end after the message's args. Returns the file descriptors it did not take.
runGetMessage :: (Message m) => Word16 -> [Fd] -> BS.ByteString -> Either MessageError (m, [Fd])
runGetMessage opcode fds body = case runWireGet (getMessage opcode) fds body of
  (Left err, _, _) -> Left err
  (Right message, rest, leftover)
    | BS.null rest -> Right (message, leftover)
    | otherwise -> Left TrailingBytes

-- | Everything needed to advertise and construct one interface.
data InterfaceEntry = InterfaceEntry
  { version :: WlUInt
  , construct :: ObjectID -> IO SomeObject
  }

type ProtocolTable = [(WlText, InterfaceEntry)]

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
  , interfaceTable :: Map WlText InterfaceEntry
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
  , objects :: IORef (Map ObjectID SomeObject)
  -- ^ Mutable `Map` of `objectID`s to interfaces they represent.
  , globals :: IORef (Map GlobalName (WlText, WlUInt))
  -- ^ `Map` of global interfaces and their versions advertised by a compositor.
  , interfaceTable :: Map WlText InterfaceEntry
  -- ^ Table of interfaces supported by the client. This allows for adding in custom protocols.
  , eventHandlers :: IORef [EventHandler p]
  -- ^ Custom event handlers that run on received events.
  , fdQueue :: TQueue Fd
  -- ^ Stores file descriptors.
  }

-- | Error saying: The connection is over. The peer reported a violation with @wl_display.error@,
-- or sent something invalid. This represents a wire value so it only carries wire types. (no `TObjectID` or `ErrorCode`)
data ProtocolError = ProtocolError
  { object :: WlObjectID
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

-- | Error saying: A message failed validation before it was sent. Nothing reached the peer and
-- the connection is still usable.
newtype InvalidMessage = InvalidMessage ProtocolError
  deriving stock (Show)

instance Exception InvalidMessage where
  displayException (InvalidMessage e) = "message not sent: " <> displayException e

-- | Error saying: The compositor does not advertise a global the client requires.
newtype MissingGlobal = MissingGlobal WlText
  deriving stock (Show)

instance Exception MissingGlobal where
  displayException (MissingGlobal iface) = "the compositor does not advertise the required global " <> show iface

-- | The connection could not be made or was lost.
data ConnectionError
  = NoSocket FilePath
  | Disconnected
  deriving stock (Show)
  deriving anyclass (Exception)

{-# WARNING in "x-stub" stub "Handled by a stub: this message is not fully implemented." #-}

-- | Placeholder for a message which is not fully implemented yet.
-- Avoid using this function if possible. Instead implement an interface fully or ditch it.
-- Partial interfaces are worse than an unimplemented one because they have inconsistent behaviour.
-- Use this instead of just `pass` or only forwarding as it warns at compile time and logs at runtime.
stub :: (Message m, HasField "wlid" i (TObjectID i)) => i -> m -> Wayland p ()
stub obj msg = liftIO . traceIO $ "sayland: unimplemented: " <> showMessage (toObjectID obj.wlid) msg

-- | Placeholder for a message which are not implemented and not planned to be due to being deprecated.
stubDeprecated :: (Message m, HasField "wlid" i (TObjectID i)) => i -> m -> Wayland p ()
stubDeprecated obj msg = liftIO . traceIO $ "sayland: unimplemented(deprecated): " <> showMessage (toObjectID obj.wlid) msg

-- | Like `sendMessage` but meant for use inside handlers.
-- It contains the logic determining if a message should be sent from the current perspective.
forwardMessage :: (Message m, HasField "wlid" i (TObjectID i)) => i -> m -> Wayland p ()
forwardMessage i m = do
  me <- getPerspective
  when (sender m == me) $ sendMessage m i.wlid

-- | Which side this is.
getPerspective :: Wayland p Perspective
getPerspective =
  asks $ \case
    ClientEnv{} -> Client
    ClientServerEnv{} -> Server

-- | 'catch' for the Wayland monad.
catchW :: (Exception e) => Wayland p a -> (e -> Wayland p a) -> Wayland p a
catchW act handler = do
  env <- ask
  liftIO $ runReaderT act env `catch` \e -> runReaderT (handler e) env

-- | How much of what breaks the protocol the library rejects. Ordered from least to most strict.
-- Only matters for `Recoverable` violations: `Unrecoverable` ones are always rejected.
data Strictness
  = -- | Reject only the violations marked @Recoverable Lenient@.
    Lenient
  | -- | Reject every violation the library detects.
    Pedantic
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Whose message broke the protocol. Each has its own `Strictness`.
data Culprit
  = -- | The peer's: a message this side received.
    Peer
  | -- | This side's own: a message it is about to send.
    Own
  deriving stock (Show, Eq)

-- | The strictness this side runs with for messages of the given culprit.
-- Stub: always `Pedantic`, until the strictness is configurable.
getStrictness :: Culprit -> Wayland p Strictness
getStrictness _ = pure Pedantic

type role Severity nominal

-- | How bad a protocol violation is. Its type is what `protocolViolation` returns.
data Severity r where
  -- | This side cannot carry on: always rejected, and `protocolViolation` does not return.
  Unrecoverable :: Severity a
  -- | This side can carry on: rejected from the given strictness up.
  -- Otherwise `protocolViolation` logs it and returns, and the caller carries on with its fallback.
  Recoverable :: Strictness -> Severity ()

-- | Report that a message broke the protocol. The one place handlers report protocol errors.
--
-- A rejected violation throws a `ProtocolError`. In the peer's message that ends the connection.
-- In this side's own message, `sendMsg` turns it into an `InvalidMessage` and nothing is sent.
--
-- > Event_wl_display_delete_id n -> case mkWlObjectID n of
-- >   Just oid -> forget oid
-- >   Nothing -> protocolViolation (Recoverable Pedantic) msg display Err_invalid_object [wl|delete_id for the null object|]
-- >
-- > surface <- getInterface surfaceId >>= maybe (protocolViolation Unrecoverable msg obj Err_invalid_object [wl|no such surface|]) pure
protocolViolation :: (Message m, ToObjectID o, ErrorCode e) => Severity r -> m -> o -> e -> WlText -> Wayland p r
protocolViolation severity msg o code text = case severity of
  Unrecoverable -> reject
  Recoverable rejectFrom -> do
    me <- getPerspective
    let culprit = if sender msg == me then Own else Peer
    strictness <- getStrictness culprit
    if strictness >= rejectFrom
      then reject
      else liftIO . traceIO $ "sayland: ignored protocol violation in " <> whose culprit <> " on object " <> show (toObjectID o) <> ": " <> show text
  where
    reject :: Wayland p a
    reject = throwIO $ ProtocolError (Just $ toObjectID o) (errorCode code) (Just text)
    whose Peer = "a message from the peer"
    whose Own = "this side's own message, which is sent anyway,"

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
  liftIO $ sendRaw socket' o (getOpcode e) (putMessage e)
