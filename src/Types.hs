{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{- |
Module      : Types
Description : various types.
Copyright   : 2018 Philipp Mayer & Engelbrecht Nils
License     : BSD3
Maintainer  : Philipp Mayer
-}
module Types where

--------------------------------------------------------------------------------
import Data.Text (Text, unpack, pack)
import qualified Network.WebSockets as WS
--------------------------------------------------------------------------------

-- | Client. Game competitor.
type Client = (Text, WS.Connection, Int)

-- | ServerState. A list of game competitors.
type ServerState = [Client]

-- | Draw. logs the diced result, the logged result and the result of the last draw.
type Draw = (Int, Int, Int)

-- | Action. A list of actions/commands a client can enter.
type Action = [(String, Int, String)]

-- | ActiveGame. Information if game is active.
type ActiveGame = Bool
