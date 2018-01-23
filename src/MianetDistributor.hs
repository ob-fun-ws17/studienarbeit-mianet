{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
-- | A MianetDistributor module.
module MianetDistributor
where

import Helper
import MianetGetter
import Types
import Distributor
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

sendToSenderClient :: WS.Connection -> Text -> IO ()
sendToSenderClient conn message = do
    WS.sendTextData conn (message :: Text)

sendToActor :: MVar ServerState -> Text -> IO ()
sendToActor stateMVar msg = do
    state <- readMVar stateMVar
    sendToClient 0 msg state

sendToReactor :: MVar ServerState -> Text -> IO ()
sendToReactor stateMVar msg = do
    state <- readMVar stateMVar
    sendToClient 1 msg state

sendToAllClients :: MVar ServerState -> Text -> IO ()
sendToAllClients stateMVar msg = do
    state <- readMVar stateMVar
    broadcast msg state

sendToAllClientsExceptSender :: Client -> MVar ServerState -> Text -> IO ()
sendToAllClientsExceptSender client stateMVar msg = do
    state <- readMVar stateMVar
    broadcastExceptSender msg client state

sendToAllExceptActor :: MVar ServerState -> Text -> IO ()
sendToAllExceptActor stateMVar msg = do
    state <- readMVar stateMVar
    actor <- getActorName stateMVar
    broadcastExceptOf msg [actor] state

sendToAllExceptActorAndReactor :: MVar ServerState -> Text -> IO ()
sendToAllExceptActorAndReactor stateMVar msg = do
    state <- readMVar stateMVar
    actor <- getActorName stateMVar
    reactor <- getReactorName stateMVar
    broadcastExceptOf msg [actor, reactor] state

sendToWinner :: MVar ServerState -> Int -> Text -> IO ()
sendToWinner stateMVar maxScore msg = do
    state <- readMVar stateMVar
    winner <- getWinner stateMVar maxScore
    WS.sendTextData (snd' winner) msg

sendToLooser :: MVar ServerState -> Int -> Text -> IO ()
sendToLooser stateMVar maxScore msg = do
    state <- readMVar stateMVar
    winnerName <- getwinnerName stateMVar maxScore
    broadcastExceptOf msg [winnerName] state
