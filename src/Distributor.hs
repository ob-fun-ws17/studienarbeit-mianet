{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
-- | A Distributor module.
module Distributor
where

import Helper
import Types
import Data.Maybe
import Data.Text (Text, unpack, pack)
import Data.List.Split
import GHC.Generics
import Control.Applicative
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

-- | removes a client from the connection list.
removeClient :: Client -> ServerState -> ServerState
removeClient client clients =
    filter (\(a,_, _) -> a /= fst' client) clients

-- | sends a message to all clients.
broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    forM_ clients $ \(_, conn, _) -> WS.sendTextData conn message

-- | sends a message to all client except of the sender client (last active client).
broadcastExceptSender :: Text -> Client -> ServerState -> IO ()
broadcastExceptSender message client clients = do
    forM_ newClients $ \(_, conn, _) -> WS.sendTextData conn message
    where
        newClients = removeClient client clients

-- | sends a message to a specific client
sendToClient :: Int -> Text -> ServerState -> IO ()
sendToClient index message clients = do
    WS.sendTextData conn message
    where
        conn = snd' $ (!!) clients index

-- | broadcast to all clients except of a specific client
broadcastExceptOf :: Text -> [Text] -> ServerState -> IO ()
broadcastExceptOf message exceptions clients = do
    forM_ newClients $ \(_, conn, _) -> WS.sendTextData conn message
    where
        newClients = filter (\(name,_,_) -> not (any (\y -> name == y) exceptions)) clients

-- | send a message to the list client in the connection list.
sendToLastClient :: Text -> ServerState -> IO ()
sendToLastClient message clients = do
    WS.sendTextData conn message
    where
        conn = snd' $ last clients
