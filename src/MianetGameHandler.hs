{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
-- | A MianetGameHandler module.
module MianetGameHandler where

--------------------------------------------------------------------------------
import Helper
import MianetGetter
import MianetDraws
import Types
import Dice
import System.Random.Shuffle
import MianetDistributor
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

-- | creates a random order for game.
shuffleOrder :: MVar ServerState -> IO (MVar ServerState)
shuffleOrder stateMVar = do
    modifyMVar_ stateMVar $ \s -> do
        s' <- shuffleM s
        return s'
    return stateMVar

-- | initiates a rematch.
rematch :: MVar ServerState -> MVar Draw -> Int -> IO ()
rematch stateMVar lastDrawMVar maxScore = do
    resetLastDraw lastDrawMVar
    resetGamersScore stateMVar
    shuffledStateMVar <- shuffleOrder stateMVar
    nextDraw shuffledStateMVar lastDrawMVar 0 maxScore

-- | resets the last draw mvar variable. for example if somebody has lied.
resetLastDraw :: MVar Draw -> IO ()
resetLastDraw lastDrawMVar = do
    modifyMVar_ lastDrawMVar $ \s -> do
        let s' = (0, 0, 0)
        return s'

-- | resets the gamers score.
resetGamersScore :: MVar ServerState -> IO ()
resetGamersScore stateMVar = do
    modifyMVar_ stateMVar $ \s -> do
        let s' = map (\(a, b, c) -> (a, b, c - c)) s
        return s'

-- | closes a game.
closeGame :: MVar ServerState -> MVar ActiveGame -> IO ()
closeGame stateMVar activeGameMVar = do
    sendToAllClients stateMVar "Auf Wiedersehen!"
    modifyMVar_ stateMVar $ \s -> do
        let s' = []
        return s'
    modifyMVar_ activeGameMVar $ \s -> do
        let s' = False
        return s'
