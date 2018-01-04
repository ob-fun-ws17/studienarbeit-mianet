{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where
import Rules
import System.IO
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
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Data.Map (Map)
import qualified Data.Map as Map


type Client = (Text, WS.Connection, Int)

type ServerState = [Client]

type Draw = (Int, Int, Int)

 --(commandName, coundOfparameter)
type Action = [(String, Int)]
-----------------------------------------------------
dice :: String
dice = "123456"

           
actions :: Action
actions = [
            ("rolldices", 0), 
            ("chat", 1), 
            ("actor", 1), 
            ("reactor", 1), 
            ("chatall", 1), 
            ("list", 0),
            ("list", 0),
            ("logfstresult", 0),
            ("nextdraw", 0),
            ("listresult", 0),
            ("logresult", 1),
            ("accuse", 0),
            ("getwin", 1),
            ("getwinner", 0)
            ]

newServerState :: ServerState
newServerState = []

lastDrawBase :: (Int, Int, Int)
lastDrawBase = (0, 0, 0)
-----------------------------------------------------

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst' client) . fst')

clientIsActor :: Client -> ServerState -> Bool
clientIsActor client clients = argument1 == argument2
    where 
        argument1 = fst' client
        argument2 = fst' $ last clients

clientIsActor' :: Client -> ServerState -> IO ()
clientIsActor' client clients = print (show argument1 ++ show argument2)
    where 
        argument1 = unpack $ fst' client
        argument2 = unpack $ fst' $ head clients

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

listClients :: ServerState -> IO ()
listClients clients = 
    forM_ clients $ \(name, _, _) -> print name

removeClient :: Client -> ServerState -> ServerState
removeClient client clients = 
    filter (\(a,_, _) -> a /= fst' client) clients

incrementWinCountForClient :: Text -> ServerState -> ServerState
incrementWinCountForClient clientName clients = 
    --filter (\(a,_, _) -> a /= clientName) clients
    map (\(a, b, c) -> if a == clientName then (a, b, c + 1) else (a, b, c)) clients

moveClient' :: ServerState -> IO ()
moveClient' clients = 
    forM_ newClients2 $ \(name, _, _) -> print name
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

sendToLastClient :: Text -> ServerState -> IO ()
sendToLastClient message clients = do
    WS.sendTextData conn message
    where
        conn = snd' $ last clients

getClientName :: Int -> ServerState -> String
getClientName index clients = unpack $ fst' $ (!!) clients index

prompt :: String -> IO String
prompt myString = do
    putStr myString
    hFlush stdout
    getLine

validPort :: String -> Bool
validPort port = 
    all (\x -> any (\y -> x == y) (intArrayToString [0..9])) port || length port == 0

intArrayToString :: [Int] -> String
intArrayToString myArray =
    foldl addToString [] myArray
    where 
        addToString myArray element = (head $ show element) : myArray

main :: IO ()
main = do
    doIt
    where 
        doIt = do
            port <- prompt "Port eingeben: "
            if validPort port
                then startServer port
                else doIt
        startServer port = do
            lastDrawMVar <- newMVar lastDrawBase
            stateMVar <- newMVar newServerState
            WS.runServer "127.0.0.1" (read port) $ application stateMVar lastDrawMVar



application :: MVar ServerState -> MVar Draw -> WS.ServerApp
application stateMVar lastDrawMVar pending = do
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
                sendToSenderClient conn ("User already exists" :: Text)

            | command == "login" -> flip finally disconnect $ do

               
               modifyMVar_ stateMVar $ \s -> do
                   let s' = addClient client s
                   WS.sendTextData conn $
                       "Welcome! Users: " `mappend`
                       T.intercalate ", " (map fst' s)
                   broadcast (fst' client `mappend` " joined") s'
                   return s' 
                
               talk conn stateMVar client lastDrawMVar

            | otherwise -> 
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

talk :: WS.Connection -> MVar ServerState -> Client -> MVar Draw -> IO ()
talk conn stateMVar (user, _, win) lastDrawMVar = forever $ do
    clients <- readMVar stateMVar
    let clients' = clients
    
    msg <- WS.receiveData conn

    case (msg) of
        _   | any ($ pack command)
                [T.null, T.any isPunctuation, T.any isSpace] ->
                    WS.sendTextData conn ("command cannot " `mappend`
                        "contain punctuation or whitespace, and " `mappend`
                        "cannot be empty" :: Text)

            | not (any (\x -> fst x == command) actions) ->
                WS.sendTextData conn ("unknow command" :: Text)
                
            | if ((snd $ head $ filter (\x -> fst x == command) actions) > 0)
                then any ($ pack parameter)
                    [T.null] else False ->
                        WS.sendTextData conn ("parameter cannot " `mappend`                        
                            "be empty" :: Text)
            
            | otherwise -> do
                handleMessage
                --readMVar stateMVar >>= handleMessage message client lastDrawMVar'
                --sendToSenderClient conn ("Unknown Action" :: Text)

          where
            messageContainer = jsonToMessageContainer $ jsonParse $ unpack msg
            message = extractContainer messageContainer
            command = getCommandOfMessage message 
            parameter = getParameterOfMessage message
            client     = (user, conn, win)
            getCmdName i = fst $ (!!) actions i   
            handleMessage = do
                state <- readMVar stateMVar  
                lastDraw <- readMVar lastDrawMVar  
                doHandleMessage message client (lastDraw, lastDrawMVar) (state, stateMVar)
            

doHandleMessage :: MS.Message -> Client -> (Draw, MVar Draw) -> (ServerState, MVar ServerState) -> IO ()
doHandleMessage message client (lastDraw, lastDrawMVar) (state, stateMVar)  = 
    case command of
        --rolldices
     _  | (command == getCmdName 0) -> 
            if isActor
                then 
                    if fst' lastDraw == 0
                        then rollDices (sendToActor') (lastDrawMVar)
                        else sendToSenderClient' ("Du hast bereits gewürfelt" :: Text)    
                else sendToSenderClient' ("Du bist nicht am Zug" :: Text)
        --chat
        | (command == getCmdName 1) -> 
            sendToAllClientsExceptSender' $ (fst' client `mappend` ": " `mappend` pack parameter)
        
        --actor
        | (command == getCmdName 2) -> 
            sendToActor' $ (fst' client `mappend` ": " `mappend` pack parameter)
        
        --reactor
        | (command == getCmdName 3) -> 
            sendToReactor' $ (fst' client `mappend` ": " `mappend` pack parameter)
        
        --chatall
        | (command == getCmdName 4) -> 
            sendToAllClients' $ (fst' client `mappend` ": " `mappend` pack parameter)
        
        --list
        | (command == getCmdName 5) -> 
            listClients state

        --logfstresult
        | (command == getCmdName 7) -> 
            logFstResult

        --nextdraw    
        | command == getCmdName 8 ->
            if clientName == reactorName
                then 
                    case lastDraw of
                        _  | fst' lastDraw == 0 ->
                                sendToReactor' (actorName `mappend` " hat noch nicht gewürfelt")
    
                            | snd' lastDraw == 0 ->
                                sendToReactor' (actorName `mappend` " hat sein Ergebnis nocht nicht eingeloggt")

                            | snd' lastDraw == 0 ->
                                sendToReactor' (actorName `mappend` " hat sein Ergebnis nocht nicht eingeloggt")

                            
    
                            | otherwise -> 
                                nextDraw $ snd' lastDraw
                    
                else sendToSenderClient' ("du bist nicht der Reactor" :: Text)

        --listresult
        | (command == getCmdName 9) -> 
            if clientName == reactorName
                then 
                    case lastDraw of
                        _  | fst' lastDraw == 0 ->
                                sendToReactor' (actorName `mappend` " hat noch nicht gewürfelt")
    
                            | snd' lastDraw == 0 ->
                                sendToReactor' (actorName `mappend` " hat sein Ergebnis nocht nicht eingeloggt")
                                
                            | otherwise ->
                                accuse 
                    
                else sendToSenderClient' ("du bist nicht der Reactor" :: Text)
        
        --logresult
        | (command == getCmdName 10) -> 
            if isActor
                then 
                    case lastDraw of
                    _  | fst' lastDraw == 0 ->
                            sendToActor' "du musst noch würfeln"

                        | snd' lastDraw /= 0 ->
                            sendToActor' "du hast bereits dein Ergebnis eingeloggt"

                        | otherwise ->
                            logResult
                            
                            
                         
                else sendToSenderClient' ("du bist nicht am Zug" :: Text)
        
        --accuse      
        | (command == getCmdName 11) ->
            accuse
        
        --getwin
        | (command == getCmdName 12) -> do
            let filterByName = filter (\(name, _, _) -> (==) name $ (pack parameter)) state
            if length (filter (\(name, _, _) -> (==) name $ (pack parameter)) state) /= 0
                then sendToSenderClient' $ pack $ show $ thd' $ head filterByName
                else sendToSenderClient' ("User nicht vorhanden" :: Text)

        | otherwise -> sendToSenderClient' ("unknow command" :: Text)
        

    where 
        sendToSenderClient' msg = WS.sendTextData (snd' client) msg
        sendToAllClients' msg = broadcast msg state
        sendToActor' msg = sendToClient 0 msg state
        sendToNextActor' msg = sendToLastClient msg state
        sendToReactor' msg = sendToClient 1 msg state
        sendToAllClientsExceptSender' msg = broadcastExceptSender msg client state
        
        actorName = fst' $ head state
        clientName = fst' client
        reactorName = fst' $ (!!) state 1
        nextActorName = fst' $ last state

        parameter = getParameterOfMessage message
        command = getCommandOfMessage message 
        getCmdName i = fst $ (!!) actions i
        lastDrawContent = do
            result <- readMVar lastDrawMVar
            print result
                   
        logFstResult = 
            modifyMVar_ lastDrawMVar $ \s -> do
                let s' = (1, 1, 1)
                return s'

        nextDraw lastRoundWin = do
            modifyMVar_ lastDrawMVar $ \s -> do   
                if lastRoundWin == 2100
                    then do                        
                        sendToActor' $ pack (accuseDefaultMessage `mappend` ". Du gewinnst")
                        return (0, 0, 0)
                    else 
                        return (0, 0, lastRoundWin) 
            moveToNextDraw
            sendToNextActor' "Du bist am Zug"
        
        moveToNextDraw =
            modifyMVar_ stateMVar $ \s -> do
                let s' = moveClient s
                return s'  

        isActor = actorName == clientName
        logResult = 
            if validResult
                then do  
                    let loggedResult' = formatNums (head loggedResult) ((!!) loggedResult 1)
                    if loggedResult' <= thd' lastDraw
                        then do
                            sendToActor' $ pack ("Error: Eingeloggtes Ergebnis muss " ++
                                "größer sein als das Ergebnis der letzten Runde (" ++
                                (show $ deformatNums $ thd' lastDraw) ++ ").")
                        else do
                            sendToReactor' $ pack
                                ((unpack actorName) ++
                                " hat gewürfelt. Würfelergebnis: " ++
                                (show $ deformatNums $ loggedResult') ++
                                ". Ergebnis der letzten Runde: " ++
                                (show $ deformatNums $ thd' lastDraw) ++
                                ". Lüge? (ja: accuse / nein: nextdraw)")
                            modifyMVar_ lastDrawMVar $ \s -> do
                                let s' = (fst' s, loggedResult', thd' s)
                                return s'
                else sendToActor' "unvalid result"
                
            
        validResult = (length parameter == 2) && (all (\x -> any (\y -> y == x) dice) parameter)
        loggedResult = foldl addToListWithConv ([] :: [Int]) parameter
        accuse = do
            if fst' lastDraw > thd' lastDraw && fst' lastDraw == snd' lastDraw
                then do
                    sendToActor' $ pack (accuseDefaultMessage `mappend` ". Du gewinnst")
                    sendToReactor' $ pack (accuseDefaultMessage `mappend` ". Du verlierst")
                    modifyMVar_ stateMVar $ \s -> do
                        let s' = incrementWinCountForClient actorName s
                        return s'
                    --nextDraw 0
                    --sendToAllClients' nextActorName `mappend` " ist am Zug."
                else do
                    sendToActor' $ pack (accuseDefaultMessage `mappend` ". Du verlierst")
                    sendToReactor' $ pack (accuseDefaultMessage `mappend` ". Du gewinnst")
                    modifyMVar_ stateMVar $ \s -> do
                        let s' = incrementWinCountForClient reactorName s
                        return s'
                    --nextDraw 0

            nextDraw 0

        accuseDefaultMessage = "Ergebnis der letzten Runde: " `mappend` (show $ deformatNums $ thd' lastDraw) `mappend`
                        ". gewürfeltes Ergebnis: " `mappend` (show $ deformatNums $ fst' lastDraw) `mappend`
                        ". eingeloggtes Ergebnis: " `mappend` (show $ deformatNums $ snd' lastDraw)
        
       

charToString :: Char -> String
charToString c = [c]

addToListWithConv :: [Int] -> Char -> [Int]
addToListWithConv myList myChar = (read (charToString myChar) :: Int) : myList               
        
sendToSenderClient :: WS.Connection -> Text -> IO ()
sendToSenderClient conn message = WS.sendTextData conn (message :: Text) 

rollDices :: (Text -> IO ()) -> MVar Draw -> IO ()
rollDices sendFunc lastDrawMVar = do
    roll1 <- rollDice
    roll2 <- rollDice
    let nums = formatNums roll1 roll2     
    result <- modifyMVar_ lastDrawMVar $ \s -> do
        let s' = (nums , 0, thd' s)
        return s'    
    sendFunc $ pack $ show $ deformatNums $ nums
            

formatNums :: Int -> Int -> Int
formatNums num1 num2 =
    case (num1, num2) of
      _ | (num1 == 1 && num2 == 2) ->
            100 * (10 * num2 + num1)

        | (num1 == 2 && num2 == 1) ->
            100 * (10 * num1 + num2)
        
        | (num1 > num2) ->
            (10 * num1 + num2)
        
        | (num1 < num2) ->
            (10 * num2 + num1)

        | (num1 == num2) ->
            10 * (10 * num1 + num2) 

deformatNums :: Int -> Int
deformatNums myNum =
    case (myNum) of
      _ | (myNum == 2100) ->
            myNum `div` 100

        | (myNum > 100) ->
            myNum `div` 10
        
        | otherwise ->
            myNum



fst' :: (a, b, c) -> a
fst' (a, _, _) = a

snd' :: (a, b, c) -> b
snd' (_, b, _) = b

thd' :: (a, b, c) -> c
thd' (_, _, c) = c       
