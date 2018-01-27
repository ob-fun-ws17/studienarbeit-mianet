{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{- |
Module      : MianetDraws
Description : This module implements the logic of the game
Copyright   : 2018 Philipp Mayer & Engelbrecht Nils
License     : BSD3
Maintainer  : Philipp Mayer
-}
module MianetDraws where
--------------------------------------------------------------------------------
import Helper
import MianetGetter
import Types
import Dice
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

-- | moves the first client in the connection list to the last position.
moveClient :: ServerState -> ServerState
moveClient clients = tail clients ++ [head clients]

-- | the reactor accused the actor of lying. it returns the result.
accuse :: MVar ServerState -> MVar Draw -> Int -> IO ()
accuse stateMVar lastDrawMVar maxScore = do
            lastDraw <- readMVar lastDrawMVar
            if fst' lastDraw > thd' lastDraw && fst' lastDraw == snd' lastDraw
                then do
                    actorName <- getActorName stateMVar
                    defaultMessage <- getDefaultMessage lastDrawMVar
                    sendToActor stateMVar (defaultMessage `mappend` ". Runde beendet. Du gewinnst")
                    sendToReactor stateMVar (defaultMessage `mappend` ". Runde beendet. Du verlierst")
                    sendToAllExceptActorAndReactor stateMVar (defaultMessage `mappend` ". Runde beendet. " `mappend` actorName `mappend` "gewinnt")
                    incrementScore stateMVar getActorName

                else do
                    reactorName <- getReactorName stateMVar
                    defaultMessage <- getDefaultMessage lastDrawMVar
                    sendToActor stateMVar (defaultMessage `mappend` ". Runde beendet. Du verlierst")
                    sendToReactor stateMVar (defaultMessage `mappend` ". Runde beendet. Du gewinnst")
                    sendToAllExceptActorAndReactor stateMVar (defaultMessage `mappend` ". Runde beendet. " `mappend` reactorName `mappend` " gewinnt")
                    incrementScore stateMVar getReactorName

            nextDraw stateMVar lastDrawMVar 0 maxScore

-- | increments the score of the actor or reactor.
incrementScore :: MVar ServerState -> (MVar ServerState -> IO Text) -> IO ()
incrementScore stateMVar func = do
    name <- func stateMVar
    modifyMVar_ stateMVar $ \s -> do
        let s' = incrementWinCountForClient name s
        return s'

-- | increments the score of a specific client.
incrementWinCountForClient :: Text -> ServerState -> ServerState
incrementWinCountForClient clientName clients =
    --filter (\(a,_, _) -> a /= clientName) clients
    map (\(a, b, c) -> if a == clientName then (a, b, c + 1) else (a, b, c)) clients

-- | initiates the next draw and sends the respective messages to the clients.
nextDraw :: MVar ServerState -> MVar Draw -> Int -> Int -> IO ()
nextDraw stateMVar lastDrawMVar lastRoundWin maxScore = do

    if lastRoundWin == 2100
        then do
            defaultMessage <- getDefaultMessage lastDrawMVar
            sendToActor stateMVar ("Würfelergebnis: 21!. Runde beendet. Du gewinnst")
            sendToAllExceptActor stateMVar ("Würfelergebnis: 21!. Runde beendet. Du verlierst")
            incrementScore stateMVar getActorName
            endOfGame <- checkForWin stateMVar maxScore
            doNextDraw endOfGame stateMVar
        else do
            endOfGame <- checkForWin stateMVar maxScore
            doNextDraw endOfGame stateMVar

    where
        doNextDraw endOfGame stateMVar = do
            if not endOfGame
                then do
                    modifyMVar_ lastDrawMVar $ \s -> do
                        if lastRoundWin == 2100
                            then do
                                --defaultMessage <- getDefaultMessage lastDrawMVar
                                --incrementScore stateMVar getActorName
                                --sendToActor stateMVar (defaultMessage `mappend` ". Runde beendet. Du gewinnst")
                                return (0, 0, 0)
                            else
                                return (0, 0, lastRoundWin)

                    stateMVar' <- moveToNextDraw stateMVar
                    order <- getOrder stateMVar
                    sendToActor stateMVar' $ "Du bist am Zug. Du musst Würfeln (rolldices). " `mappend` order
                    sendToAllExceptActor stateMVar' $ order `mappend` " warten..."
                else do
                    endOfGameFunc stateMVar maxScore

-- | initiates the next draw. it calls the moveClient function.
moveToNextDraw :: MVar ServerState -> IO (MVar ServerState)
moveToNextDraw stateMVar = do
    modifyMVar_ stateMVar $ \s -> do
        let s' = moveClient s
        return s'
    return stateMVar

-- | rolls dices two times and returns the result.
rollDices' :: MVar Draw -> IO Int
rollDices' lastDrawMVar = do
    roll1 <- rollDice
    roll2 <- rollDice
    let nums = formatNums roll1 roll2
    result <- modifyMVar_ lastDrawMVar $ \s -> do
        let s' = (nums , 0, thd' s)
        return s'
    return nums

-- | checks if somebody got the max score.
checkForWin :: MVar ServerState -> Int -> IO Bool
checkForWin stateMVar maxScore = do
    state <- readMVar stateMVar
    return (any (\(_, _, win) -> win == maxScore) state)

-- | inititates the end of a game.
endOfGameFunc :: MVar ServerState -> Int -> IO ()
endOfGameFunc stateMVar maxScore = do
    state <- readMVar stateMVar
    winnerName <- getwinnerName stateMVar maxScore
    sendToWinner stateMVar maxScore "Spiel beendet. Du gewinnst!"
    sendToLooser stateMVar maxScore ("Spiel beendet. " `mappend` winnerName `mappend` " hat gewonnen")
    -- sendToActor stateMVar "Spiel beendet. Du gewinnst!"
    -- broadcastExceptOf msg [actor] state  stateMVar ("Spiel beendet. " `mappend` actorN `mappend` " hat gewonnen")
    sendToAllClients stateMVar ("nochmal: (rematch). Spiel verlassen: (closeGame)")

-- | it logs the entered dice result of the actor.
logResult :: MVar ServerState -> MVar Draw -> String -> IO ()
logResult stateMVar lastDrawMVar parameter =
    if validResult parameter
        then do
            lastDraw <- readMVar lastDrawMVar
            let loggedResult' = formatNums (head loggedResult) ((!!) loggedResult 1)
            if loggedResult' <= thd' lastDraw
                then do
                    lastDraw <- readMVar lastDrawMVar
                    sendToActor stateMVar $ pack ("Error: Eingeloggtes Ergebnis muss " ++
                        "größer sein als das Ergebnis der letzten Runde (" ++
                        (show $ deformatNums $ thd' lastDraw) ++ ").")
                else do
                    lastDraw <- readMVar lastDrawMVar
                    reactorN <- getReactorName stateMVar
                    actorN <- getActorName stateMVar
                    order <- getOrder stateMVar
                    let resultMessage = (unpack actorN) ++
                            " hat gewürfelt. Würfelergebnis: " ++
                            (show $ deformatNums $ loggedResult') ++
                            ". Ergebnis der letzten Runde: " ++
                            (show $ deformatNums $ thd' lastDraw)

                    sendToReactor stateMVar $ pack (resultMessage ++ ". Lüge? (ja: accuse / nein: nextdraw)")

                    sendToActor stateMVar $ "warten auf " `mappend` reactorN
                    sendToAllExceptActorAndReactor stateMVar (pack $ resultMessage ++ ". " ++ (unpack order) ++ ". warten...")

                    modifyMVar_ lastDrawMVar $ \s -> do
                        let s' = (fst' s, loggedResult', thd' s)
                        return s'
        else sendToActor stateMVar "unvalid result"
    where
        loggedResult = foldl addToListWithConv ([] :: [Int]) parameter


-- | checks if it's a valid result.
validResult :: String -> Bool
validResult parameter = (length parameter == 2) && (all (\x -> any (\y -> y == x) dice) parameter)

-- | converts a char to a list of integer and puts it to another list of integer.
addToListWithConv :: [Int] -> Char -> [Int]
addToListWithConv myList myChar = (read ([myChar]) :: Int) : myList

-- | in process.
addToListWithConv' :: [Int] -> Char -> Maybe [Int]
addToListWithConv' myList myChar =
    case myChar of
        _ | any (\x -> [myChar] == x) numsAsString -> Just readList
          | otherwise -> Nothing

    where
        readList = (read ([myChar]) :: Int) : myList
        numsAsString = map (\x -> show x) [0..9]
