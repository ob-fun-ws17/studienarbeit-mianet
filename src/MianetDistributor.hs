{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{- |
Module      : MianetDistributor
Description : This module is used for distributing messages with various recipients.
Copyright   : BSD3
License     : 2018 Philipp Mayer & Engelbrecht Nils
Maintainer  : Philipp Mayer
-}
module MianetDistributor where

--------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------

-- | sends a message to the last active client.
sendToSenderClient :: WS.Connection -> Text -> IO ()
sendToSenderClient conn message = do
    WS.sendTextData conn (message :: Text)

-- | sends a message to the actor client.
sendToActor :: MVar ServerState -> Text -> IO ()
sendToActor stateMVar msg = do
    state <- readMVar stateMVar
    sendToClient 0 msg state

-- | sends a message to the reactor client.
sendToReactor :: MVar ServerState -> Text -> IO ()
sendToReactor stateMVar msg = do
    state <- readMVar stateMVar
    sendToClient 1 msg state

-- | sends a message to all clients
sendToAllClients :: MVar ServerState -> Text -> IO ()
sendToAllClients stateMVar msg = do
    state <- readMVar stateMVar
    broadcast msg state

-- | sends a message to all clients except of the sender client (last active client).
sendToAllClientsExceptSender :: Client -> MVar ServerState -> Text -> IO ()
sendToAllClientsExceptSender client stateMVar msg = do
    state <- readMVar stateMVar
    broadcastExceptSender msg client state

-- | sends a message to all clients except of the actor client.
sendToAllExceptActor :: MVar ServerState -> Text -> IO ()
sendToAllExceptActor stateMVar msg = do
    state <- readMVar stateMVar
    actor <- getActorName stateMVar
    broadcastExceptOf msg [actor] state

-- | sends a message to all clients except of actor and reactor.
sendToAllExceptActorAndReactor :: MVar ServerState -> Text -> IO ()
sendToAllExceptActorAndReactor stateMVar msg = do
    state <- readMVar stateMVar
    actor <- getActorName stateMVar
    reactor <- getReactorName stateMVar
    broadcastExceptOf msg [actor, reactor] state


-- | sends a message to winners (winner client).
sendToWinner :: MVar ServerState -> Int -> Text -> IO ()
sendToWinner stateMVar maxScore msg = do
    state <- readMVar stateMVar
    winner <- getWinner stateMVar maxScore
    WS.sendTextData (snd' winner) msg

-- | send a message to the loosers.
sendToLooser :: MVar ServerState -> Int -> Text -> IO ()
sendToLooser stateMVar maxScore msg = do
    state <- readMVar stateMVar
    winnerName <- getwinnerName stateMVar maxScore
    broadcastExceptOf msg [winnerName] state
