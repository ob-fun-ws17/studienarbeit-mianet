{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
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

removeClient :: Client -> ServerState -> ServerState
removeClient client clients = 
    filter (\(a,_, _) -> a /= fst' client) clients

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    T.putStrLn message
    forM_ clients $ \(_, conn, _) -> WS.sendTextData conn message

broadcastExceptSender :: Text -> Client -> ServerState -> IO ()
broadcastExceptSender message client clients = do
    T.putStrLn message
    forM_ newClients $ \(_, conn, _) -> WS.sendTextData conn message
    where 
        newClients = removeClient client clients

sendToClient :: Int -> Text -> ServerState -> IO ()
sendToClient index message clients = do
    WS.sendTextData conn message
    where
        conn = snd' $ (!!) clients index

broadcastExceptOf :: Text -> [Text] -> ServerState -> IO ()
broadcastExceptOf message exceptions clients = do
    T.putStrLn message
    forM_ newClients $ \(_, conn, _) -> WS.sendTextData conn message
    where 
        newClients = filter (\(name,_,_) -> not (any (\y -> name == y) exceptions)) clients

sendToLastClient :: Text -> ServerState -> IO ()
sendToLastClient message clients = do
    WS.sendTextData conn message
    where
        conn = snd' $ last clients