{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
-- | Types module.
module Types
where

import Data.Text (Text, unpack, pack)
import qualified Network.WebSockets as WS

-- | Client type.
type Client = (Text, WS.Connection, Int)

-- | ServerState type.
type ServerState = [Client]

-- | Draw type.
type Draw = (Int, Int, Int)

-- | Action type, (commandName, coundOfparameter)
type Action = [(String, Int, String)]

-- | ActiveGame type.
type ActiveGame = Bool
