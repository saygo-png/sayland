-- | Description : Utilities that depend on protocols.
module Sayland.WaylandUtils (waylandSetup) where

import Control.Concurrent.STM (newTQueue)
import Data.Bimap qualified as BM
import Network.Socket
import Relude
import Sayland.Protocols.Wayland
import Sayland.Types
import Sayland.WaylandSocket
import Sayland.Wire.Types

waylandSetup :: ProtocolTable Client -> IO (WaylandEnv Client)
waylandSetup protocolTable = do
  let display :: Interface Client = Interface $ Wl_display wlDisplayId
  getSocketPath openSocketName >>= \case
    Just path -> do
      putStrLn $ "using socket path: " <> show path
      sock <- socket AF_UNIX Stream defaultProtocol
      connect sock $ SockAddrUnix path
      counter <- newIORef $ coerce wlDisplayId
      objects <- newIORef $ fromList [(coerce wlDisplayId, display)]
      globals <- newIORef BM.empty
      handlers <- newIORef mempty
      interfaceTable' <- newIORef $ fromList protocolTable
      fdqueue <- atomically newTQueue
      pure $ ClientEnv $ ClientEnvironment sock counter objects globals interfaceTable' handlers fdqueue
    Nothing -> error "couldn't find `$WAYLAND_DISPLAY`, nor any open socket."
