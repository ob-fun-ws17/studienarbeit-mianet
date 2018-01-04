--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Message
import           System.IO
import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text, unpack, pack)
import           Data.List.Split
import           Control.Applicative
import           Data.Aeson
import           Data.Maybe
import           GHC.Generics
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS
import qualified Data.ByteString.Lazy.Char8 as C


--------------------------------------------------------------------------------
app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"

    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg

    -- Read from stdin and write to WS
    let loop = do
            line <- T.getLine
            --line <- prompt
                      
            --json
            let unhandledMsg = if(unpack line /= "") then messageHandler line else "hallo"
            let message = pack $ unhandledMsg
            
            --let command = decode(show(message))
            let messageContainer = jsonToMessageContainer $ jsonParse unhandledMsg
            let msg = extractContainer messageContainer
            
            --print command parameter
            --print $ getCommandOfMessage msg
            
            unless (T.null line) $ WS.sendTextData conn message >> loop

    loop
    WS.sendClose conn ("Bye!" :: Text)

--------------------------------------------------------------------------------
main :: IO ()
main = do
    server <- prompt "Server-Adresse eingeben: "
    doIt server
    where
        doIt serv = do            
            port <- prompt "Port-Nr. eingeben: "
            if validPort port 
                then
                    withSocketsDo $ WS.runClient serv (read port :: Int) "/" app
                else doIt serv
    
    --withSocketsDo $ WS.runClient "127.0.0.1" 9000 "/" app



validPort :: String -> Bool
validPort port = 
    all (\x -> any (\y -> x == y) (intArrayToString [0..9])) port || length port == 0

intArrayToString :: [Int] -> String
intArrayToString myArray =
    foldl addToString [] myArray
    where 
        addToString myArray element = (head $ show element) : myArray


prompt :: String -> IO String
prompt myString = do
    putStr myString
    hFlush stdout
    getLine
        

