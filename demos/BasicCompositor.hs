-- | Description : A compositor that serves the core and xdg-shell protocols, without rendering anything.
module Main (main) where

-- THIS IS JUST A SERVER. IT DOES NOT RENDER ANYTHING.

import Control.Exception (bracket)
import Data.Map qualified as Map
import Network.Socket
import Relude
import Sayland
import Sayland.Wire
import System.Directory (removeFile)

table :: ProtocolTable
table = waylandTable <> xdg_shellTable

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
      getSocketPath availableSocketName >>= \case
        Nothing -> error "couldn't find a socket path"
        Just socketPath -> do
          socket' <- socket AF_UNIX Stream 0
          bind socket' (SockAddrUnix socketPath)
          listen socket' 5

          clients <- newTVarIO Map.empty
          eventHandlers <- newIORef []
          clientSerial <- newTVarIO 0
          pure
            ServerEnvironment
              { socket = socket'
              , socketPath
              , clients
              , interfaceTable = Map.fromList table
              , eventHandlers
              , clientSerial
              }

program :: ServerEnvironment -> IO ()
program = listenForClients
