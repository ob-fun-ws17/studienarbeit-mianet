{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
-- | Main module.
module Main where

--------------------------------------------------------------------------------
import           Control.Concurrent    (MVar, newMVar, modifyMVar_, modifyMVar, readMVar, forkIO, killThread)
import           GameInfo              (broadcastGameInfo, receiveGameInfo, returnHostPortTupel)
import           Server                (application)
import           Network.Socket        hiding (recv)
import           GHC.Conc.Sync         (ThreadId)
import qualified Network.WebSockets    as WS
import           Client                (app)
import qualified Data.ByteString.Char8 as C
import qualified Data.Text.IO          as T
import qualified Data.Text             as T
import           Data.List.Split
import           MianetConstants
import           Control.Monad
import           Network.BSD
import           Data.Maybe
import           Helper
--------------------------------------------------------------------------------

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