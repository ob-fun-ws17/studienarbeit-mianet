{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
-- | Main module.
module Main where

import Dice
import Types
import Helper
import MianetDraws
import MianetGetter
import MianetConstants
import MianetGameHandler
import System.IO
import MianetDistributor
import Distributor
import System.Random.Shuffle
import Message as MS
import Data.Maybe
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text, unpack, pack)
import Data.List.Split
import Data.Aeson
import GHC.Generics
import Control.Applicative
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar, forkIO, killThread, forkOS)
import GHC.Conc.Sync (ThreadId)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Network.BSD
import Network.Socket hiding     (recv)
import Data.Map (Map)
import qualified Data.Map as Map
import Lib (broadcastGameInfo, receiveGameInfo)
import Server
import Client
import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.List.Split

returnHostPortTupel :: String -> (String, Int)
returnHostPortTupel x = (head $ splitOn ":" x, read (last $ splitOn ":" x))

-- | Main method
main :: IO ()
main = do
    askMode
    where
        askMode = do
            answer <- prompt "Nur Spiel beitreten (join / j) oder auch erzeugen (create / c): "
            if onlyClient answer
                then do
                  t <- receiveGameInfo
                  when (isJust t) $ do
                    let msg = fromJust t
                    putStrLn $ "Spiel gefunden: " ++ (C.unpack msg)
                    let tupel = returnHostPortTupel $ C.unpack msg
                    let host = fst tupel
                    let port = snd tupel
                    withSocketsDo $ WS.runClient host port "/" app
                  when (isNothing t) $ do
                    putStrLn "Kein Spiel gefunden"
                    askMode
                else portConf
                     where
                         portConf = do
                             port <- prompt "Port eingeben: "
                             if validPort port
                                 then scoreConf port
                                 else portConf

                         scoreConf port = do
                             score <- prompt "Punkte (max): "
                             if validPort score
                                 then startServer port score
                                 else scoreConf port

                         startServer port score = do
                             lastDrawMVar <- newMVar lastDrawBase
                             stateMVar <- newMVar newServerState
                             activeGameMVar <- newMVar False
                             hostName <- getHostName
                             threadId <- forkIO $ forever (do broadcastGameInfo $ hostName ++ ":" ++ port)
                             _ <- forkIO $ WS.runServer "0.0.0.0" (read port) $ application stateMVar lastDrawMVar activeGameMVar (read score) threadId
                             withSocketsDo $ WS.runClient "127.0.0.1" (read port) "/" app
