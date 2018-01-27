{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
-- | A Message module.
module Message
  ( messageHandler
  , jsonStringify
  , jsonParse
  , jsonToMessageContainer
  , getCommandOfMessage
  , getParameterOfMessage
  , extractContainer
  , Message
  ) where

--------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------

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

-- | converts command line input into json.
messageHandler :: Text -> String
messageHandler msg = C.unpack message
    where message = encode $ Message { command = cmd, parameter = param }
          cmd = head $ cmdLineParams
          param = if (length cmdLineParams > 1) then (!!) cmdLineParams 1 else ""
          cmdLineParams = getCmdLineParams (unpack msg)

-- | handles the command line input.
getCmdLineParams :: String -> [String]
getCmdLineParams message = formatCmdLineParam $ foldl splitCmdLine ("", [], False) message

-- | splits the command line input.
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

-- | formats the command line input.
formatCmdLineParam :: (String, [String], Bool) -> [String]
formatCmdLineParam (charArray, stringArray, doubleQuotes) = filteredResult
                where
                    stringArray' = map (\x -> reverse x) stringArray
                    charArray' = reverse charArray
                    reversedResult = reverse (charArray' : stringArray' )
                    filteredResult = filter (\x -> x /= "") reversedResult

-- | takes a json object and convert it to a string.
jsonStringify :: C.ByteString -> String
jsonStringify json = C.unpack json

-- | takes a string and converts it to a json object.
jsonParse :: String -> C.ByteString
jsonParse jsonString = C.pack jsonString

-- | takes a jsonObject and puts it in a container.
jsonToMessageContainer :: C.ByteString -> Maybe Message
jsonToMessageContainer json = decode $ json :: Maybe Message

-- | returns the parameter "command" of a message.
getCommandOfMessage :: Message -> String
getCommandOfMessage msg = command msg

-- | returns the parameter "parameter" of a message.
getParameterOfMessage :: Message -> String
getParameterOfMessage msg = parameter msg

-- | takes Maybe a and returns a.
extractContainer :: Maybe a -> a
extractContainer (Just a) = a
extactContainer (Nothing) = ()
