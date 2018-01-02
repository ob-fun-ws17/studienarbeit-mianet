{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Message
( messageHandler, jsonStringify, jsonParse, jsonToMessageContainer, getCommandOfMessage, getParameterOfMessage, extractContainer
) where

--------------------------------
import           Control.Concurrent  (forkIO, MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Data.Text           (Text, unpack, pack)
import           Data.List
import           Data.List.Split
import           Control.Applicative as CA
import           Data.Aeson
import           GHC.Generics
import           Text.Regex.Posix 
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Data.ByteString.Lazy.Char8 as C

data Message = Message {
    command       :: String,
    parameter     :: String
  } deriving Show

instance FromJSON Message where
    parseJSON (Object v) = Message <$> v .: "command" <*> v .: "parameter"
    parseJSON _ = CA.empty

instance ToJSON Message where
    toJSON (Message cmd param) = object ["command" .= cmd, "parameter" .= param]


messageHandler :: Text -> String
messageHandler msg = C.unpack message
    where message = encode $ Message { command = cmd, parameter = param }
          cmd = head $ cmdLineParams
          param = if (length cmdLineParams > 1) then (!!) cmdLineParams 1 else ""
          cmdLineParams = getCmdLineParams (unpack msg)


getCmdLineParams :: String -> [String]
getCmdLineParams message = formatCmdLineParam $ foldl splitCmdLine ("", [], False) message

--formatCmdLineParam $ foldl splitCmdLine ("", [], False) "halt 'stop was' hi 'hi enton' 'jimi hendrix' 'josh' wasss"
splitCmdLine :: (String, [String], Bool) -> Char -> (String, [String], Bool)
splitCmdLine (charArray, stringArray, doubleQuotes) char = 
    if (doubleQuotes)
        then
            if (char /= '\'') 
                then
                    (char : charArray, stringArray, True)
                else                    
                    ([], charArray : stringArray, False) 
        else  
            if (char == ' ' || char == '\'') 
                then 
                    ([], charArray : stringArray, char') 
                else 
                    (char : charArray, stringArray, char') 
    where 
        char' = if (char == '\'') then True else False

formatCmdLineParam :: (String, [String], Bool) -> [String]
formatCmdLineParam (charArray, stringArray, doubleQuotes) = filteredResult
                where
                    stringArray' = map (\x -> reverse x) stringArray
                    charArray' = reverse charArray
                    reversedResult = reverse (charArray' : stringArray' )
                    filteredResult = filter (\x -> x /= "") reversedResult

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