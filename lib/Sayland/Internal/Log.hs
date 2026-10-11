{-# OPTIONS_HADDOCK not-home #-}

-- | Description : Logging.
module Sayland.Internal.Log (module Sayland.Internal.Log) where

import Control.Applicative
import Control.Exception (IOException, catch, evaluate)
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString qualified as BS
import Data.Foldable (fold)
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Sayland.Internal.Prelude
import System.Environment
import System.IO

-- | How important a log line is. Ordered from least to most.
data LogLevel = LogDebug | LogInfo | LogWarn | LogError
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Number representing a Wayland Client.
type ClientID = Int

-- | Which way a message went over the wire.
data Direction = Received | Sent
  deriving stock (Show, Eq)

-- | Where log lines go and which of them get there.
data Logger = Logger
  { minLevel :: LogLevel
  -- ^ Lines below this level are dropped.
  , write :: LogLevel -> Maybe ClientID -> String -> IO ()
  -- ^ Write out one line, tagged with the client it concerns if any. Only called from `minLevel` up.
  , writeMessage :: Direction -> Maybe ClientID -> String -> IO ()
  -- ^ Write out one message that went over the wire, at `LogDebug`. Only called if `minLevel` lets that through.
  }

-- | Log a line, unless it is below the logger's `minLevel`.
logWith :: (MonadIO m) => Logger -> LogLevel -> Maybe ClientID -> String -> m ()
logWith logger level client text = when (level >= logger.minLevel) . liftIO $ logger.write level client text

-- | Drops every line, errors included.
silentLogger :: Logger
silentLogger = Logger{minLevel = maxBound, write = \_ _ _ -> pass, writeMessage = \_ _ _ -> pass}

-- | Parsed @WAYLAND_DEBUG@: which sides log from `LogDebug` up, tracing every message.
data WaylandDebug = WaylandDebug {client :: Bool, server :: Bool}

-- | Log to stderr from `LogWarn` up, or from `LogDebug` up when @WAYLAND_DEBUG@ names the side picked by the argument.
--
-- Reads the environment, so it is NOT safe to call while another thread sets environment variables.
-- Blame C: <https://www.evanjones.ca/setenv-is-not-thread-safe.html>
defaultLoggerFor :: (WaylandDebug -> Bool) -> IO Logger
defaultLoggerFor side = do
  debugVar <- lookupEnv "WAYLAND_DEBUG"
  let parsed = parseWaylandDebug debugVar
      unrecognized = "ignoring WAYLAND_DEBUG=" <> show (fold debugVar) <> ", it is not one of: 1, client, server, 0"
  logger <- handleLogger stderr $ if maybe False side parsed then LogDebug else LogWarn
  when (isNothing parsed) $ logWith logger LogWarn Nothing unrecognized
  pure logger

-- | Parse @WAYLAND_DEBUG@. `Nothing` means it is set to an unrecognized value.
parseWaylandDebug :: Maybe String -> Maybe WaylandDebug
parseWaylandDebug = \case
  Nothing -> Just $ WaylandDebug False False
  Just v | v `elem` ["", "0"] -> Just $ WaylandDebug False False
  Just "1" -> Just $ WaylandDebug True True
  Just "client" -> Just $ WaylandDebug True False
  Just "server" -> Just $ WaylandDebug False True
  Just _ -> Nothing

-- | Log to a handle, coloring if `hShouldColor` says so.
--
-- Each line is rendered in full before anything is written, then written with a single call, so lines logged
-- from concurrent threads never interleave and a line that fails to render writes nothing.
-- Failing to write (e.g. a closed stderr) is ignored: logging must not bring the connection down.
handleLogger :: Handle -> LogLevel -> IO Logger
handleLogger h minLevel = do
  colored <- hShouldColor h
  let put line = do
        bytes <- evaluate . T.encodeUtf8 $ T.pack line
        BS.hPut h bytes `catch` \(_ :: IOException) -> pass
      write level client = put . formatLine colored level client . pure . (Nothing,)
      writeMessage direction client text = put . formatLine colored LogDebug client $ case direction of
        Received -> [(Just magenta, "<-   " <> text)]
        Sent -> [(Just green, "  -> " <> text)]
  pure Logger{minLevel, write, writeMessage}

-- | Render a log line like @WARN  [client 3] something happened@ from its text in pieces of a given color,
-- ending in a newline. Colors are plain ANSI codes: terminfo is not thread-safe, and garbles or crashes when lines
-- are logged from several threads at once.
formatLine :: Bool -> LogLevel -> Maybe ClientID -> [(Maybe String, String)] -> String
formatLine colored level client pieces =
  colorize (Just tagColor) tag <> replicate (6 - length tag) ' ' <> clientTag <> foldMap (uncurry colorize) pieces <> "\n"
  where
    colorize (Just sgr) s | colored = "\ESC[" <> sgr <> "m" <> s <> "\ESC[39m"
    colorize _ s = s
    clientTag = foldMap (\c -> "[client " <> show c <> "] ") client
    (tagColor, tag) = case level of
      LogDebug -> (cyan, "DEBUG")
      LogInfo -> (blue, "INFO")
      LogWarn -> (yellow, "WARN")
      LogError -> (red, "ERROR")

-- | ANSI foreground color codes.
red, yellow, green, blue, magenta, cyan :: String
red = "31"
green = "32"
yellow = "33"
blue = "34"
magenta = "35"
cyan = "36"

-- | Whether to color what is written to the handle: as the environment forces it (see `colorOverride`),
-- otherwise if the handle is a terminal and @TERM@ is set to something other than @dumb@.
hShouldColor :: Handle -> IO Bool
hShouldColor h = do
  isTerminal <- hIsTerminalDevice h
  term <- lookupEnv "TERM"
  fromMaybe (isTerminal && term `notElem` [Nothing, Just "", Just "dumb"]) <$> colorOverride

-- | Whether the environment forces coloring on (@Just True@) or off (@Just False@). Highest priority first.
colorOverride :: IO (Maybe Bool)
colorOverride =
  asum
    <$> sequenceA
      [ -- https://force-color.org/
        -- According to its example C code, this takes priority over @NO_COLOR@.
        rule "FORCE_COLOR" (/= "") True
      , -- https://no-color.org/
        -- According to cmake, @NO_COLOR@ takes priority over @CLICOLOR_FORCE@.
        rule "NO_COLOR" (/= "") False
      , -- https://cmake.org/cmake/help/latest/envvar/CLICOLOR_FORCE.html
        -- According to cmake, @CLICOLOR_FORCE@ takes priority over @CLICOLOR@.
        rule "CLICOLOR_FORCE" (/= "0") True
      , -- https://cmake.org/cmake/help/latest/envvar/CLICOLOR.html
        rule "CLICOLOR" (== "0") False
      ]
  where
    -- If the variable is set and active (per the predicate) the rule decides the verdict. Otherwise it abstains.
    rule name isActive verdict = (>>= \v -> verdict <$ guard (isActive v)) <$> lookupEnv name
