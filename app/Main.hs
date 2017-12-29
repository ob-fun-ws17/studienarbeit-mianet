{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where
import Rules
import Message as MS
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text, unpack, pack)
import Data.List.Split
import Data.Aeson
import GHC.Generics
import Control.Applicative
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

type Client = (Text, WS.Connection)


type ServerState = [Client]


newServerState :: ServerState
newServerState = []


numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

clientIsActor :: Client -> ServerState -> Bool
clientIsActor client clients = argument1 == argument2
    where 
        argument1 = fst client
        argument2 = fst $ last clients

clientIsActor' :: Client -> ServerState -> IO ()
clientIsActor' client clients = print (show argument1 ++ show argument2)
    where 
        argument1 = unpack $ fst client
        argument2 = unpack $ fst $ head clients

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

listClients :: ServerState -> IO ()
listClients clients = 
    forM_ clients $ \(name, _) -> print name

removeClient :: Client -> ServerState -> ServerState
removeClient client clients = 
    filter (\(a,_) -> a /= fst client) clients

moveClient' :: ServerState -> IO ()
moveClient' clients = 
    forM_ newClients2 $ \(name, _) -> print name
    where 
        newClients1 = last clients : clients
        newClients2 = init newClients1

moveClient :: ServerState -> ServerState
moveClient clients =
    init newClients1
    where 
        newClients1 = last clients : clients

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    T.putStrLn message
    forM_ clients $ \(_, conn) -> WS.sendTextData conn message

broadcastExceptSender :: Text -> Client -> ServerState -> IO ()
broadcastExceptSender message client clients = do
    T.putStrLn message
    forM_ newClients $ \(_, conn) -> WS.sendTextData conn message
    where 
        newClients = removeClient client clients

sendToFirstClient :: Text -> ServerState -> IO ()
sendToFirstClient message clients = do
    WS.sendTextData fstConn message
    where
        fstConn = snd $ head clients

sendToSecondClient :: Text -> ServerState -> IO ()
sendToSecondClient message clients = do
    WS.sendTextData sndConn message
    where
        sndConn = snd $ (!!) clients 0

sendToClient :: Int -> Text -> ServerState -> IO ()
sendToClient index message clients = do
    WS.sendTextData conn message
    where
        conn = snd $ (!!) clients index

getClientName :: Int -> ServerState -> String
getClientName index clients = unpack $ fst $ (!!) clients index

   



main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9000 $ application state


application :: MVar ServerState -> WS.ServerApp


application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

    msg <- WS.receiveData conn
    clients <- readMVar state
    
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
                sendToSenderClient conn ("User already exists" :: Text)


            | command == "login" -> flip finally disconnect $ do


               modifyMVar_ state $ \s -> do
                   let s' = addClient client s
                   WS.sendTextData conn $
                       "Welcome! Users: " `mappend`
                       T.intercalate ", " (map fst s)
                   broadcast (fst client `mappend` " joined") s'
                   return s' 
                
               talk conn state client

            | otherwise -> 
                sendToSenderClient conn ("Unknown Action" :: Text)

          where
            messageContainer = jsonToMessageContainer $ jsonParse $ unpack msg
            message = extractContainer messageContainer
            command = getCommandOfMessage message 
            parameter = getParameterOfMessage message
            client     = (pack parameter, conn)
            disconnect = do
                -- Remove client and return new state
                s <- modifyMVar state $ \s ->
                    let s' = removeClient client s in return (s', s')
                broadcast (fst client `mappend` " disconnected") s


talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
talk conn state (user, _) = forever $ do
    clients <- readMVar state
    let clients' = clients
    
    msg <- WS.receiveData conn

    case (msg) of
        _   | any ($ pack command)
                [T.null, T.any isPunctuation, T.any isSpace] ->
                    WS.sendTextData conn ("parameter cannot " `mappend`
                        "contain punctuation or whitespace, and " `mappend`
                        "cannot be empty" :: Text)

            | any ($ pack parameter)
                [T.null, T.any isPunctuation, T.any isSpace] ->
                    WS.sendTextData conn ("command cannot " `mappend`
                        "contain punctuation or whitespace, and " `mappend`
                        "cannot be empty" :: Text)
            
            | command == "nextdraw" ->
                 modifyMVar_ state $ \s -> do
                     let s' = moveClient s
                     return s'
             
            | otherwise ->
                readMVar state >>= handleMessage message client
                --sendToSenderClient conn ("Unknown Action" :: Text)

          where
            messageContainer = jsonToMessageContainer $ jsonParse $ unpack msg
            message = extractContainer messageContainer
            command = getCommandOfMessage message 
            parameter = getParameterOfMessage message
            client     = (user, conn)            

handleMessage :: MS.Message -> Client -> ServerState -> IO ()
handleMessage message client state = 
    case command of
        "rolldices" -> 
            if actorName == clientName
                then rollDices sendToActor'
                else sendToSenderClient' ("Du bist nicht am Zug" :: Text)
        "chat" ->
            sendToAllClientsExceptSender' $ (fst client `mappend` ": " `mappend` pack parameter)

        "actor" ->
            sendToActor' $ (fst client `mappend` ": " `mappend` pack parameter)

        "reactor" ->
            sendToReactor' $ (fst client `mappend` ": " `mappend` pack parameter)

        "chatall" ->
            sendToAllClients' $ (fst client `mappend` ": " `mappend` pack parameter)

        "list" -> 
            listClients state

        _ -> sendToSenderClient' ("unknow command" :: Text)
        --"chat" ->
            --sendToAllClients'

    where 
        sendToSenderClient' msg = WS.sendTextData (snd client) msg
        sendToAllClients' msg = broadcast msg state
        sendToActor' msg = sendToClient 0 msg state
        sendToReactor' msg = sendToClient 1 msg state
        sendToAllClientsExceptSender' msg = broadcastExceptSender msg client state
        
        actorName = fst $ head state
        clientName = fst client
        parameter = getParameterOfMessage message
        command = getCommandOfMessage message 


sendToSenderClient :: WS.Connection -> Text -> IO ()
sendToSenderClient conn message = WS.sendTextData conn (message :: Text) 

sendToAllClients :: MVar ServerState -> Text -> IO ()
sendToAllClients state message = readMVar state >>= broadcast message

sendToActor :: MVar ServerState -> Text -> IO ()
sendToActor state message = readMVar state >>= sendToClient 0 message

sendToReactor :: MVar ServerState -> Text -> IO ()
sendToReactor state message = readMVar state >>= sendToClient 1 message

sendToAllClientsExceptSender :: MVar ServerState -> Client -> Text -> IO ()
sendToAllClientsExceptSender state client message = readMVar state >>= broadcastExceptSender message client


rollDices :: (Text -> IO ()) -> IO ()
rollDices func = do
    roll1 <- rollDice
    roll2 <- rollDice
    func $ formatDiceResult roll1 roll2
    where
        formatDiceResult roll1 roll2 = pack $ show roll1 ++ show roll2
 