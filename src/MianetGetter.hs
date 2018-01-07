{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module MianetGetter
where

import Helper
import Types
import Dice
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

getActor :: MVar ServerState -> IO Client
getActor stateMVar = do
    state <- readMVar stateMVar
    return $ head state

getReactor :: MVar ServerState -> IO Client
getReactor stateMVar = do
    state <- readMVar stateMVar
    return $ (!!) state 1

getActorName :: MVar ServerState -> IO Text
getActorName stateMVar = do   
    actor <- getActor stateMVar 
    return $ fst' $ actor

getReactorName :: MVar ServerState -> IO Text
getReactorName stateMVar = do   
    reactor <- getReactor stateMVar 
    return $ fst' $ reactor

getWinner :: MVar ServerState -> Int -> IO Client
getWinner stateMVar maxScore = do
    state <- readMVar stateMVar
    let winner = head $ filter (\x -> thd' x == maxScore) state
    return winner

getwinnerName :: MVar ServerState -> Int -> IO Text
getwinnerName stateMVar maxScore = do
    winner <- getWinner stateMVar maxScore
    let winnerName = fst' winner
    return winnerName

getOrder :: MVar ServerState -> IO Text
getOrder stateMVar = do
    state <- readMVar stateMVar
    return ("(" `mappend` (T.intercalate "->" (map (\x -> fst' x) state)) `mappend` ")")

getDefaultMessage :: MVar Draw -> IO Text
getDefaultMessage lastDrawMVar = do
    lastDraw <- readMVar lastDrawMVar
    return
        ("Ergebnis der letzten Runde: " `mappend` (pack $ show $ deformatNums $ thd' lastDraw) `mappend`
        ". gewürfeltes Ergebnis: " `mappend` (pack $ show $ deformatNums $ fst' lastDraw) `mappend`
        ". eingeloggtes Ergebnis: " `mappend` (pack $ show $ deformatNums $ snd' lastDraw))


getClientName :: Int -> ServerState -> String
getClientName index clients = unpack $ fst' $ (!!) clients index
