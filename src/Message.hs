{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
-- | A Message module.
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

-- | Message data.
data Message = Message {
    command       :: String,
    parameter     :: String
  } deriving Show

-- | FromJSON Message instance.
instance FromJSON Message where
    parseJSON (Object v) = Message <$> v .: "command" <*> v .: "parameter"
    parseJSON _ = empty

-- | ToJSON Message instance.
instance ToJSON Message where
    toJSON (Message cmd param) = object ["command" .= cmd, "parameter" .= param]

-- | takes text and returns String.
messageHandler :: Text -> String
messageHandler msg = C.unpack message
    where message = encode $ Message { command = cmd, parameter = param }
          cmd = head $ splitOn " " $ unpack msg
          param = last $ splitOn " " $ unpack msg

-- | takes ByteString and returns String.
jsonStringify :: C.ByteString -> String
jsonStringify json = C.unpack json

-- | takes String and return ByteString.
jsonParse :: String -> C.ByteString
jsonParse jsonString = C.pack jsonString

-- | takes ByteString and returns Maybe Message.
jsonToMessageContainer :: C.ByteString -> Maybe Message
jsonToMessageContainer json = decode $ json :: Maybe Message

-- | takes Message and returns commands to String.
getCommandOfMessage :: Message -> String
getCommandOfMessage msg = command msg

-- | takes Message and returns parameters as String.
getParameterOfMessage :: Message -> String
getParameterOfMessage msg = parameter msg

-- | takes Maybe a and returns a.
extractContainer :: Maybe a -> a
extractContainer (Just a) = a
extactContainer (Nothing) = ()
