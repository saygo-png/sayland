-- | Description : Utilities that depend on protocols
module Sayland.WaylandUtils (waylandSetup) where

import Control.Concurrent.STM (newTQueue)
import Data.Bimap qualified as BM
import Network.Socket hiding (openSocket)
import Protocol (InterfaceClientTable, VersionTable)
import Relude
import Sayland.Protocols.Wayland
import Sayland.Types
import Sayland.WaylandSocket

waylandSetup :: InterfaceClientTable -> VersionTable -> IO (WaylandEnv Client)
waylandSetup interfaceTable versionTable = do
  let display :: Interface Client = Interface $ Wl_display wlDisplayId
  getSocketPath openSocket >>= \case
    Just path -> do
      putStrLn $ "using socket path: " <> show path
      sock <- socket AF_UNIX Stream defaultProtocol
      connect sock $ SockAddrUnix path
      counter <- newIORef $ coerce wlDisplayId
      objects <- newIORef $ fromList [(coerce wlDisplayId, display)]
      globals <- newIORef BM.empty
      handlers <- newIORef mempty
      interfaceTable' <- newIORef $ fromList interfaceTable
      versionTable' <- newIORef $ fromList versionTable
      fdqueue <- atomically newTQueue
      pure $ ClientEnv $ ClientEnvironment sock counter objects globals interfaceTable' versionTable' handlers fdqueue
    Nothing -> error "couldn't find `$WAYLAND_DISPLAY`, nor any open socket."
