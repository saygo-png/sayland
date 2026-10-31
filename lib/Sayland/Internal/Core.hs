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
import Data.Foldable (for_)
import Data.Function
import Data.Functor
import Data.Kind
import Data.Map (Map)
import Data.Typeable
import GHC.Records (HasField)
import Network.Socket (Socket)
import Sayland.Internal.Log
import Sayland.Internal.Prelude
import Sayland.Wire
import System.IO.Error (isResourceVanishedError)

-- | Type representing an `objectID` of a certain object.
newtype TObjectID (a :: Type) = TObjectID AnyObjectID deriving newtype (Show, Eq, Ord)

type role TObjectID phantom

-- | Internal class for converting shit to a rawObjectID.
class ToObjectID o where
  -- | The ID of an object, or of a t'TObjectID'.
  toObjectID :: o -> AnyObjectID

instance ToObjectID (TObjectID a) where
  toObjectID = coerce

-- forces UndecidableInstances and OVERLAPPABLE x_x
instance {-# OVERLAPPABLE #-} (Interface o) => ToObjectID o where
  toObjectID o = toObjectID o.wlid

-- | The Wayland monad. Allows easy access to the Wayland environment state without threading repetitive arguments.
type Wayland p = ReaderT (WaylandEnv p) IO

-- | Type representing global names which are numbers.
-- Created in order to prevent mixups between object ids and global names.
newtype GlobalName = GlobalName Word32
  deriving newtype (Show, Eq, Ord, Num, Enum)

-- | An interface a compositor advertises as a global, which clients bind with @wl_registry.bind@.
class (Object i) => Global i where
  -- | Construct the object bound to this ID. The default suits interfaces whose only field is @wlid@.
  global :: TObjectID i -> IO i
  default global :: (Coercible (TObjectID i) i) => TObjectID i -> IO i
  global = pure . coerce

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
  -- | Effect of a request on local state. Returns the messages it wants to send in t`Outbox`. Runs on the sender and the receiver.
  onRequest :: i -> Request i -> Wayland p Outbox

  -- | Effect of an event on local state. Returns the  messages it wants to send in t`Outbox`. Runs on the sender and the receiver.
  onEvent :: i -> Event i -> Wayland p Outbox

-- | Resolves which handler a message reaches, given the perspective it arrives from.
class KnownPerspective (p :: Perspective) where
  -- | Route an incoming message to 'onEvent' on a client, 'onRequest' on a server.
  applyIncoming :: (Object i) => i -> Incoming p i -> Wayland p Outbox

  -- | Route an outgoing message to 'onRequest' on a client, 'onEvent' on a server.
  applyOutgoing :: (Object i) => i -> Outgoing p i -> Wayland p Outbox

  -- | Typeclass bs. Bring the 'Message' instance of the messages this side receives for @i@ into scope.
  withIncoming :: (Object i) => i -> ((Message (Incoming p i)) => Wayland p r) -> Wayland p r

  -- | An ID this side allocated, as an `AnyObjectID`.
  toAnyObjectID :: ObjectID p -> AnyObjectID

  -- | The first ID this side allocates for a new object.
  firstObjectID :: ObjectID p

instance KnownPerspective Client where
  applyIncoming = onEvent
  applyOutgoing = onRequest
  withIncoming _ k = k -- Incoming Client i = Event i, Message via Interface
  toAnyObjectID = FromClient
  firstObjectID = mkObjectID @Client @2 -- 1 is always wl_display.

instance KnownPerspective Server where
  applyIncoming = onRequest
  applyOutgoing = onEvent
  withIncoming _ k = k -- Incoming Server i = Request i, Message via Interface
  toAnyObjectID = FromServer
  firstObjectID = mkObjectID @Server @0xff000000

type role EventHandler nominal

-- | EventHandlers, called whenever an event is received.
data EventHandler p where
  EventHandler :: (Typeable m, Message m) => (AnyObjectID -> m -> Wayland p ()) -> EventHandler p

-- | Class defining an Interface as a collection of events and requests which has a t'TObjectID', version and name.
-- This does not include implementations of events and requests which are supplied by `Interface`.
class
  ( Message (Event a)
  , Message (Request a)
  , HasField "wlid" a (TObjectID a)
  , Typeable a
  ) =>
  Interface a
  where
  -- | The events of the interface, sent by the server.
  type Event a = r | r -> a

  -- | The requests of the interface, sent by the client.
  type Request a = r | r -> a

  getInterfaceVersion :: Proxy a -> Word32
  getInterfaceName :: Proxy a -> WlText

-- | The ordered messages a handler wants to send. FIFO ordering.
-- What ran the handler ends up sending the messages (see `sendOutbox`).
-- This is done to prevent messages being sent inside a handler, just to then have it report a violation after having already sent something.
-- It also allows callers of handlers to respond to state changes before a peer, preventing races.
-- The type is kept abstract in case a list structure here is inefficient. Handlers are a hot path since they run on all messages.
newtype Outbox = Outbox [Queued]
  deriving newtype (Semigroup, Monoid)

-- | Some message to be sent to an object.
data Queued where
  Queued :: (Message m) => AnyObjectID -> m -> Queued

-- | Existential type for holding different objects in one structure.
data SomeObject where
  SomeObject :: (Object i) => i -> SomeObject

-- | The events or the requests of an interface, which go over the wire.
class (Typeable m) => Message m where
  -- | Which side sends the message.
  sender :: m -> Perspective

  -- | Get the message with this opcode.
  getMessage :: Word16 -> WireGet (Either MessageError m)

  -- | Put the message's arguments.
  putMessage :: m -> WirePut ()

  -- | The opcode of the message.
  getOpcode :: m -> Word16

  -- | Show the message as sent to the given object, for logs.
  showMessage :: AnyObjectID -> m -> String

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
    UnknownEnumValue Word32
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
  { version :: Word32
  , construct :: AnyObjectID -> IO SomeObject
  }

-- | The interfaces of a protocol by name. Each protocol module generates one, like @waylandTable@.
-- Combine them with '<>'.
type ProtocolTable = [(WlText, InterfaceEntry)]

type role WaylandEnv nominal

-- | What the Wayland monad runs in: this side of one connection. On a server, also the server and which client it is.
data WaylandEnv (p :: Perspective) where
  ClientEnv :: ConnectionEnv Client -> WaylandEnv Client
  ClientServerEnv :: ServerEnv -> ConnectionEnv Server -> ClientID -> WaylandEnv Server

-- | The state of a server: its socket, its connections to clients, and what it serves.
data ServerEnv = ServerEnv
  { listenSocket :: Socket
  -- ^ The socket clients connect to.
  , socketPath :: FilePath
  , clients :: TVar (Map ClientID (ConnectionEnv Server))
  -- ^ Currently connected clients.
  , clientSerial :: TVar ClientID
  -- ^ Client counter, for identifying individual clients.
  , interfaceTable :: Map WlText InterfaceEntry
  -- ^ Table of interfaces supported by the server. This allows for adding in custom protocols.
  , eventHandlers :: IORef [EventHandler Server]
  -- ^ Server-side event handlers.
  , onRecoverable :: OnRecoverable
  -- ^ What to do with the `Recoverable` protocol violations of the server and its clients.
  , serverLogger :: Logger
  -- ^ Where the server and its clients log to.
  }

type role ConnectionEnv nominal

-- | The state of one connection, on either side: on a client its connection to the compositor,
-- on a server its connection to one client.
data ConnectionEnv (p :: Perspective) = ConnectionEnv
  { connectionSocket :: Socket
  -- ^ The socket of this connection.
  , writeLock :: MVar ()
  -- ^ write lock used to avoid race conditions.
  , counter :: MVar (Maybe (ObjectID p))
  -- ^ The next `ObjectID` this side allocates. `Nothing` once its range is used up.
  , nextPeerID :: IORef Word32
  -- ^ The highest ID the peer may give a new object: one past the highest it has used.
  , objects :: TVar (Map AnyObjectID SomeObject)
  -- ^ Mutable `Map` of `objectID`s to interfaces they represent.
  , globals :: IORef (Map GlobalName (WlText, Word32))
  -- ^ `Map` of global interfaces and their versions advertised by a compositor.
  , interfaceTable :: Map WlText InterfaceEntry
  -- ^ Table of interfaces supported by the client. This allows for adding in custom protocols.
  , eventHandlers :: IORef [EventHandler p]
  -- ^ Custom event handlers that run on received events.
  , fdQueue :: TQueue Fd
  -- ^ Stores file descriptors.
  , onRecoverable :: OnRecoverable
  -- ^ What to do with this connection's `Recoverable` protocol violations.
  , logger :: Logger
  -- ^ Where this connection logs to.
  }

-- | Error saying: The connection is over. The peer reported a violation with @wl_display.error@,
-- or sent something invalid. This represents a wire value so it carries no t'TObjectID' or `ErrorCode`.
data ProtocolError = ProtocolError
  { object :: WlObjectID
  , code :: Word32
  , message :: WlString
  }
  deriving stock (Show)

instance Exception ProtocolError where
  displayException e =
    "wayland protocol error on object " <> show e.object <> " (code " <> show e.code <> "): " <> show e.message

-- | Enums a protocol declares as error codes (@\<enum name="error"\>@).
class ErrorCode e where
  errorCode :: e -> Word32

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
  = NoSocket String
  | Disconnected
  deriving stock (Show)
  deriving anyclass (Exception)

{-# WARNING in "x-stub" stub "Handled by a stub: this message is not fully implemented." #-}

-- | Placeholder for a message which is not fully implemented yet.
-- Avoid using this function if possible. Instead implement an interface fully or ditch it.
-- Partial interfaces are worse than an unimplemented one because they have inconsistent behaviour.
-- Use this instead of just `pass` or only forwarding as it warns at compile time and logs at runtime.
stub :: (Message m, HasField "wlid" i (TObjectID i)) => i -> m -> Wayland p ()
stub obj msg = logWarn $ "unimplemented " <> showMessage (toObjectID obj.wlid) msg

-- | Placeholder for a message which are not implemented and not planned to be due to being deprecated.
stubDeprecated :: (Message m, HasField "wlid" i (TObjectID i)) => i -> m -> Wayland p ()
stubDeprecated obj msg = logInfo $ "unimplemented (deprecated): " <> showMessage (toObjectID obj.wlid) msg

-- | Put a message in the outbox as the sender. Returns an empty t`Outbox` if called from a receiving side.
forwardMessage :: (Message m, HasField "wlid" i (TObjectID i)) => i -> m -> Wayland p Outbox
forwardMessage i m = do
  me <- getPerspective
  pure $ if sender m == me then outgoingMessage m i.wlid else mempty

-- | Put a message into an t'Outbox' no matter the side. If you want per side behaviour look at `forwardMessage`.
outgoingMessage :: (Message m) => m -> TObjectID i -> Outbox
outgoingMessage m (TObjectID o) = Outbox [Queued o m]

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

-- | What `protocolViolation` does with a `Recoverable` violation. `Unrecoverable` ones are always rejected.
data OnRecoverable
  = -- | Reject it like an `Unrecoverable` one.
    Reject
  | -- | Log it as a warning, and carry on.
    Warn
  deriving stock (Show, Eq)

type role Severity nominal

-- | How bad a protocol violation is. Its type is what `protocolViolation` returns.
data Severity r where
  -- | This side cannot carry on: always rejected, and `protocolViolation` does not return.
  Unrecoverable :: Severity a
  -- | This side can carry on: handled as the environment's `OnRecoverable` says.
  --   If it is only logged, `protocolViolation` returns and the caller carries on with its fallback.
  Recoverable :: Severity ()

-- | Report that a message broke the protocol. The one place handlers report protocol errors.
--
-- A rejected violation throws a t'ProtocolError'. In the peer's message that ends the connection.
-- In this side's own message, 'Sayland.Object.sendMsg' turns it into an t'InvalidMessage' and nothing is sent.
--
-- > Event_wl_display_delete_id n -> case mkWlObjectID n of
-- >   Just oid -> forget oid
-- >   Nothing -> protocolViolation Recoverable msg display Err_invalid_object [wl|delete_id for the null object|]
-- >
-- > surface <- getInterface surfaceId >>= maybe (protocolViolation Unrecoverable msg obj Err_invalid_object [wl|no such surface|]) pure
protocolViolation :: (Message m, ToObjectID o, ErrorCode e) => Severity r -> m -> o -> e -> WlText -> Wayland p r
protocolViolation severity msg o code text = case severity of
  Unrecoverable -> reject
  Recoverable -> do
    policy <- getConnectionEnv <&> (.onRecoverable)
    case policy of
      Reject -> reject
      Warn -> do
        me <- getPerspective
        logWarn $ "ignored protocol violation in " <> whose (sender msg == me) <> " on object " <> show (toObjectID o) <> ": " <> show text
  where
    reject :: Wayland p a
    reject = throwIO $ ProtocolError (Just $ toObjectID o) (errorCode code) (Just text)
    whose own = if own then "this side's own message, which is sent anyway," else "a message from the peer"

-- | Helper. The value in a 'Maybe', or the fallback action if there's none.
whenNothing :: (Applicative f) => Maybe a -> f a -> f a
whenNothing m fallback = maybe fallback pure m

-- | Get the ConnectionEnv behind the Wayland monad.
getConnectionEnv :: Wayland p (ConnectionEnv p)
getConnectionEnv =
  asks $ \case
    ClientEnv env -> env
    ClientServerEnv _ env _ -> env

-- | The logger, and the client this connection serves if this is the server.
getLogger :: Wayland p (Logger, Maybe ClientID)
getLogger =
  asks $ \case
    ClientEnv env -> (env.logger, Nothing)
    ClientServerEnv _ env client -> (env.logger, Just client)

-- | Log a line at the given level. On the server, it is tagged with the client this connection serves.
logAt :: LogLevel -> String -> Wayland p ()
logAt level text = getLogger >>= \(logger, client) -> logWith logger level client text

-- | Log a line at the level in the name. See `logAt`.
logDebug, logInfo, logWarn, logError :: String -> Wayland p ()
logDebug = logAt LogDebug
logInfo = logAt LogInfo
logWarn = logAt LogWarn
logError = logAt LogError

-- | Log a message that went over the wire in the given direction, at `LogDebug`.
logMessage :: (Message m) => Direction -> AnyObjectID -> m -> Wayland p ()
logMessage direction oid msg = do
  (logger, client) <- getLogger
  when (LogDebug >= logger.minLevel) . liftIO . logger.writeMessage direction client $ showMessage oid msg

-- | Log to stderr from `LogWarn` up, or from `LogDebug` up (every message included) when @WAYLAND_DEBUG@ names
-- the given side: @1@ for both, @client@ or @server@ for one.
--
-- Reads the environment, so it is NOT safe to call while another thread sets environment variables.
-- Blame C: <https://www.evanjones.ca/setenv-is-not-thread-safe.html>
defaultLogger :: Perspective -> IO Logger
defaultLogger = \case
  Client -> defaultLoggerFor (.client)
  Server -> defaultLoggerFor (.server)

-- | Run something only on the server side. Used for implementing interfaces. Gives `mempty` on the client.
onServer :: (Monoid a) => Wayland Server a -> Wayland p a
onServer k =
  ask >>= \case
    ClientServerEnv{} -> k
    ClientEnv{} -> pure mempty

-- | Run something only on the client side. Used for implementing interfaces. Gives `mempty` on the server.
onClient :: (Monoid a) => Wayland Client a -> Wayland p a
onClient k =
  ask >>= \case
    ClientServerEnv{} -> pure mempty
    ClientEnv{} -> k

-- | Send a raw message over the wire, without running its handler.
sendMessage :: (Message m) => m -> TObjectID i -> Wayland p ()
sendMessage e o = sendOutbox $ outgoingMessage e o

-- | Send the messages of an t'Outbox' in order. Done under a write lock so messages do not mangle.
-- Each is logged as it is sent, so the log shows them in the order they go over the wire.
sendOutbox :: Outbox -> Wayland p ()
sendOutbox (Outbox messages) = do
  env <- getConnectionEnv
  (logger, client) <- getLogger
  liftIO . withMVar env.writeLock $ \_ ->
    for_ messages $ \(Queued o m) -> do
      when (LogDebug >= logger.minLevel) . logger.writeMessage Sent client $ showMessage o m
      peerGone $ sendRaw env.connectionSocket o (getOpcode m) (putMessage m)

-- | Report the socket errors that mean the peer hung up (@EPIPE@, @ECONNRESET@) as `Disconnected`.
peerGone :: IO a -> IO a
peerGone act = catchJust (guard . isResourceVanishedError) act $ \_ -> throwIO Disconnected
