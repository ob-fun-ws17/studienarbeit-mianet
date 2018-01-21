{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Message
( messageHandler, jsonStringify, jsonParse, jsonToMessageContainer, getCommandOfMessage, getParameterOfMessage, extractContainer, Message
) where

--------------------------------
import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Data.Text           (Text, unpack, pack)
import           Data.List.Split
import           Control.Applicative
import           Data.Aeson
import           GHC.Generics
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Data.ByteString.Lazy.Char8 as C

data Message = Message {
    command       :: String,
    parameter     :: String
  } deriving Show

instance FromJSON Message where
    parseJSON (Object v) = Message <$> v .: "command" <*> v .: "parameter"
    parseJSON _ = empty

instance ToJSON Message where
    toJSON (Message cmd param) = object ["command" .= cmd, "parameter" .= param]


messageHandler :: Text -> String
messageHandler msg = C.unpack message
    where message = encode $ Message { command = cmd, parameter = param }
          cmd = head $ splitOn " " $ unpack msg
          param = last $ splitOn " " $ unpack msg

jsonStringify :: C.ByteString -> String
jsonStringify json = C.unpack json

jsonParse :: String -> C.ByteString
jsonParse jsonString = C.pack jsonString

jsonToMessageContainer :: C.ByteString -> Maybe Message
jsonToMessageContainer json = decode $ json :: Maybe Message

getCommandOfMessage :: Message -> String
getCommandOfMessage msg = command msg

getParameterOfMessage :: Message -> String
getParameterOfMessage msg = parameter msg


extractContainer :: Maybe a -> a
extractContainer (Just a) = a
extactContainer (Nothing) = ()