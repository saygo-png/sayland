module Main (main) where

-- THIS IS JUST A SERVER. IT DOES NOT RENDER ANYTHING.

import Control.Exception (bracket)
import Data.Map qualified as Map
import Network.Socket
import Relude
import Sayland
import System.Directory (removeFile)

main :: IO ()
main = bracket env cleanup program
  where
    cleanup :: ServerEnvironment -> IO ()
    cleanup senv = do
      clients <- readTVarIO senv.clients
      mapM_ (\client -> close client.socket) clients
      close senv.socket
      removeFile senv.socketPath
    env :: IO ServerEnvironment
    env = do
      getSocketPath availableSocket >>= \case
        Nothing -> error "couldn't find a socket path"
        Just socketPath -> do
          socket' <- socket AF_UNIX Stream 0
          bind socket' (SockAddrUnix socketPath)
          listen socket' 5

          clients <- newTVarIO Map.empty
          interfaceTable <- newIORef $ Map.fromList waylandInterfaceServerTable
          versionTable <- newIORef $ Map.fromList waylandVersionTable
          eventHandlers <- newIORef []
          clientSerial <- newTVarIO 0
          pure
            ServerEnvironment
              { socket = socket'
              , socketPath
              , clients
              , interfaceTable
              , versionTable
              , eventHandlers
              , clientSerial
              }

program :: (MonadIO m) => ServerEnvironment -> m ()
program = listenForClients
