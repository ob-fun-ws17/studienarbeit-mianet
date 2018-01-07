{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Types
where

import Data.Text (Text, unpack, pack)
import qualified Network.WebSockets as WS

type Client = (Text, WS.Connection, Int)

type ServerState = [Client]

type Draw = (Int, Int, Int)

 --(commandName, coundOfparameter)
type Action = [(String, Int, String)]

type ActiveGame = Bool