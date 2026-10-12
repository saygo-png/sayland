-- | Description : A compositor that serves the core and xdg-shell protocols, without rendering anything.
module Main (main) where

-- THIS IS JUST A SERVER. IT DOES NOT RENDER ANYTHING.

import Control.Exception (bracket)
import Relude
import Sayland

table :: ProtocolTable
table = waylandTable <> xdg_shellTable

main :: IO ()
main = bracket (waylandServerSetup table) waylandServerClose listenForClients
