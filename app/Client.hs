--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-|
Module      : Client
Description : Client module used to start the Client WS
Copyright   : BSD3
License     : 2018 Philipp Mayer & Engelbrecht Nils
Maintainer  : Nils Engelbrecht
-}
module Client
    ( app
    ) where

--------------------------------------------------------------------------------
import           Message
import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text, unpack, pack)
import           Control.Applicative
import           Data.Aeson
import           GHC.Generics
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS
import qualified Data.ByteString.Lazy.Char8 as C
--------------------------------------------------------------------------------

app :: WS.ClientApp ()
app conn = do
    putStrLn "Bitte Login durchführen (login <username>)"
    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg

    -- Read from stdin and write to WS
    let loop = do
            line <- T.getLine

            --json
            let unhandledMsg = if(unpack line /= "") then messageHandler line else "" --"hallo"
            let message = pack $ unhandledMsg

            --let command = decode(show(message))
            let messageContainer = jsonToMessageContainer $ jsonParse unhandledMsg
            let msg = extractContainer messageContainer
            unless (T.null line) $ WS.sendTextData conn message >> loop
    loop
    WS.sendClose conn ("Bye!" :: Text)