{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
-- | Server module.
module Server where

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
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar, forkIO, killThread)
import GHC.Conc.Sync (ThreadId)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Network.BSD
import Network.Socket hiding     (recv)
import Data.Map (Map)
import qualified Data.Map as Map
import Lib (broadcastGameInfo)

-----------------------------------------------------

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst' client) . fst')

clientIsActor :: Client -> ServerState -> Bool
clientIsActor client clients = argument1 == argument2
    where
        argument1 = fst' client
        argument2 = fst' $ last clients

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

listClients :: ServerState -> IO ()
listClients clients =
    forM_ clients $ \(name, _, _) -> print name

-- | A method to start the webserver.
application :: MVar ServerState -> MVar Draw -> MVar ActiveGame -> Int -> ThreadId -> WS.ServerApp
application stateMVar lastDrawMVar activeGameMVar maxScore threadId pending = do
    hFlush stdout
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    clients <- readMVar stateMVar

    case msg of
        _   | any ($ pack parameter)
                [T.null, T.any isPunctuation, T.any isSpace] ->
                    sendToSenderClient conn ("parameter cannot " `mappend`
                        "contain punctuation or whitespace, and " `mappend`
                        "cannot be empty" :: Text)

            | any ($ pack command)
                [T.null, T.any isPunctuation, T.any isSpace] ->
                    sendToSenderClient conn ("command cannot " `mappend`
                        "contain punctuation or whitespace, and " `mappend`
                        "cannot be empty" :: Text)


            | clientExists client clients ->
                sendToSenderClient conn ("Der User exisitiert bereits" :: Text)

            | command == "login" -> flip finally disconnect $ do
               activeGame <- readMVar activeGameMVar
               if not activeGame
                then do
                    modifyMVar_ stateMVar $ \s -> do
                        let s' = addClient client s
                        WS.sendTextData conn $
                            "Herzlich Willkommen! Spieler-Liste: " `mappend`
                            T.intercalate ", " (map fst' s)
                        broadcast (fst' client `mappend` " ist dem Spiel beigetreten. Spiel starten mit (start)") s'
                        return s'

                    talk conn stateMVar client lastDrawMVar activeGameMVar maxScore threadId
                else sendToSenderClient conn "game already started"


            | otherwise -> do
                sendToSenderClient conn ("Unknown Action" :: Text)

          where
            messageContainer = jsonToMessageContainer $ jsonParse $ unpack msg
            message = extractContainer messageContainer
            command = getCommandOfMessage message
            parameter = getParameterOfMessage message
            client     = (pack parameter, conn, 0)
            disconnect = do
                -- Remove client and return new stateMVar
                s <- modifyMVar stateMVar $ \s ->
                    let s' = removeClient client s in return (s', s')
                broadcast (fst' client `mappend` " disconnected") s

-- | The talk method.
talk :: WS.Connection -> MVar ServerState -> Client -> MVar Draw -> MVar ActiveGame -> Int -> ThreadId ->IO ()
talk conn stateMVar (user, _, win) lastDrawMVar activeGameMVar maxScore threadId = forever $ do
    clients <- readMVar stateMVar
    let clients' = clients
    hFlush stdout
    msg <- WS.receiveData conn

    case (msg) of
        _   | any ($ pack command)
                [T.null, T.any isPunctuation, T.any isSpace] ->
                    WS.sendTextData conn ("command cannot " `mappend`
                        "contain punctuation or whitespace, and " `mappend`
                        "cannot be empty" :: Text)

            | not (any (\x -> fst' x == command) actions) ->
                WS.sendTextData conn ("unknow command" :: Text)

            | if ((snd' $ head $ filter (\x -> fst' x == command) actions) > 0)
                then any ($ pack parameter)
                    [T.null] else False ->
                        WS.sendTextData conn ("parameter cannot " `mappend`
                            "be empty" :: Text)

            --start
            | command == getCmdName 11 -> do
                activeGame <- readMVar activeGameMVar
                if not activeGame
                    then do
                        state <- readMVar stateMVar
                        if length state > 1
                            then do
                                modifyMVar_ activeGameMVar $ \s -> do
                                    let s' = True
                                    return s'

                                -- modifyMVar_ stateMVar $ \s -> do
                                --     s' <- shuffleM s
                                --     return s'

                                -- kills broadcasting threadId because game is being started
                                killThread threadId
                                shuffledStateMVar <- shuffleOrder stateMVar

                                order <- getOrder shuffledStateMVar
                                state <- readMVar shuffledStateMVar
                                sendToAllClients shuffledStateMVar "Spiel wurde gestartet!"
                                sendToActor shuffledStateMVar $ "Du bist am Zug. Du musst Würfeln (rolldices). " `mappend` order
                                sendToAllExceptActor shuffledStateMVar $ order `mappend` " warten..."
                            else sendToSenderClient conn "Es müssen mindestens 2 Spieler teilnehmen. Spiel starten mit (start)"
                    else sendToSenderClient conn "Das Spiel wurde bereits gestartet"

            -- closeGame
            | (command == getCmdName 13) -> do
                closeGame stateMVar activeGameMVar
                resetLastDraw lastDrawMVar

            -- rematch
            | (command == getCmdName 14) -> do
                rematch stateMVar lastDrawMVar maxScore


            | otherwise -> do
                activeGame <- readMVar activeGameMVar
                if activeGame
                    then handleMessage
                    else sendToSenderClient conn "Das Spiel wurde noch nicht gestartet. Spiel starten (start)"
                -- readMVar stateMVar >>= handleMessage message client lastDrawMVar'
                -- sendToSenderClient conn ("Unknown Action" :: Text)

          where
            messageContainer = jsonToMessageContainer $ jsonParse $ unpack msg
            message = extractContainer messageContainer
            command = getCommandOfMessage message
            parameter = getParameterOfMessage message
            client     = (user, conn, win)
            getCmdName i = fst' $ (!!) actions i
            handleMessage = do
                state <- readMVar stateMVar
                lastDraw <- readMVar lastDrawMVar
                doHandleMessage message client (lastDraw, lastDrawMVar) (state, stateMVar) maxScore

-- | handleMessage method.
doHandleMessage :: MS.Message -> Client -> (Draw, MVar Draw) -> (ServerState, MVar ServerState) -> Int -> IO ()
doHandleMessage message client (lastDraw, lastDrawMVar) (state, stateMVar) maxScore  =
    case command of
        -- rolldices
     _  | (command == getCmdName 5) ->
            if isActor
                then
                    if fst' lastDraw == 0
                        then rollDices
                        else sendToSenderClient clientConn ("Du hast bereits gewürfelt" :: Text)
                else sendToSenderClient clientConn ("Du bist nicht am Zug" :: Text)
        -- chat
        | (command == getCmdName 1) ->
            sendToAllClientsExceptSender client stateMVar $ (fst' client `mappend` ": " `mappend` pack parameter)

        -- actor
        | (command == getCmdName 2) ->
            sendToActor stateMVar $ (fst' client `mappend` ": " `mappend` pack parameter)

        -- reactor
        | (command == getCmdName 3) ->
            sendToReactor stateMVar $ (fst' client `mappend` ": " `mappend` pack parameter)

        -- chatall
        | (command == getCmdName 4) ->
            sendToAllClients stateMVar $ (fst' client `mappend` ": " `mappend` pack parameter)


        -- nextdraw
        | command == getCmdName 9 ->
            if clientName == reactorName
                then
                    case lastDraw of
                        _  | fst' lastDraw == 0 ->
                                sendToReactor stateMVar (actorName `mappend` " hat noch nicht gewürfelt")

                            | snd' lastDraw == 0 ->
                                sendToReactor stateMVar (actorName `mappend` " hat sein Ergebnis nocht nicht eingeloggt")

                            | snd' lastDraw == 0 ->
                                sendToReactor stateMVar (actorName `mappend` " hat sein Ergebnis nocht nicht eingeloggt")



                            | otherwise -> do
                                lastDraw <- readMVar lastDrawMVar
                                nextDraw stateMVar lastDrawMVar (snd' lastDraw) maxScore

                else sendToSenderClient clientConn ("du bist nicht der Reactor" :: Text)

        -- accuse
        | (command == getCmdName 10) ->
            if clientName == reactorName
                then
                    case lastDraw  of
                        _  | fst' lastDraw == 0 ->
                                sendToReactor stateMVar (actorName `mappend` " hat noch nicht gewürfelt")

                            | snd' lastDraw == 0 ->
                                sendToReactor stateMVar (actorName `mappend` " hat sein Ergebnis nocht nicht eingeloggt")

                            | otherwise ->
                                accuse stateMVar lastDrawMVar maxScore

                else sendToSenderClient clientConn ("du bist nicht der Reactor" :: Text)

        -- logresult
        | (command == getCmdName 8) ->
            if isActor
                then
                    case lastDraw of
                    _  | fst' lastDraw == 0 ->
                            sendToActor stateMVar "du musst noch würfeln"

                        | snd' lastDraw /= 0 ->
                            sendToActor stateMVar "du hast bereits dein Ergebnis eingeloggt"

                        | otherwise ->
                            logResult stateMVar lastDrawMVar parameter



                else sendToSenderClient clientConn ("du bist nicht am Zug" :: Text)


        -- getwin
        | (command == getCmdName 6) -> do
            let filterByName = filter (\(name, _, _) -> (==) name $ (pack parameter)) state
            if length (filter (\(name, _, _) -> (==) name $ (pack parameter)) state) /= 0
                then sendToSenderClient clientConn $ pack $ show $ thd' $ head filterByName
                else sendToSenderClient clientConn ("User nicht vorhanden" :: Text)

        -- help
        | (command == getCmdName 0) -> do
            help clientConn

        | otherwise -> sendToSenderClient clientConn ("unknow command" :: Text)


    where
        clientConn = (snd' client)
        actorName = fst' $ head state
        clientName = fst' client
        reactorName = fst' $ (!!) state 1
        parameter = getParameterOfMessage message
        command = getCommandOfMessage message
        getCmdName i = fst' $ (!!) actions i
        rollDices = do
            nums <- rollDices' lastDrawMVar
            sendToActor stateMVar $ pack $ "dein Würfelergebnis: " ++ (show $ deformatNums $ nums) ++ ". Ergebnis der letzten Runde: " ++ (show $ deformatNums $ thd' lastDraw) ++ ". Ergebnis eingeben (logresult <würfelergebnis>)"
            sendToAllExceptActor stateMVar (actorName `mappend` " hat gewürfelt. warten ...")

        isActor = actorName == clientName

-- | The help method.
help :: WS.Connection -> IO ()
help clientConn = do
    sendToSenderClient clientConn $ pack $ concat $ map (\x -> fst' x ++ " --- " ++ thd' x ++ "\n") actions
