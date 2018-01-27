{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{- |
Module      : Helper
Description : Helper module.
Copyright   : 2018 Philipp Mayer & Engelbrecht Nils
License     : BSD3
Maintainer  : Philipp Mayer
-}
module Helper where
-- Helper module.
--------------------------------------------------------------------------------
import System.IO
import Data.Maybe
--------------------------------------------------------------------------------

-- | returns the first element of a 3-element-tupel
fst' :: (a, b, c) -> a
fst' (a, _, _) = a

-- | returns the second element of a 3-element-tupel
snd' :: (a, b, c) -> b
snd' (_, b, _) = b

-- | returns the third element of a 3-element-tupel
thd' :: (a, b, c) -> c
thd' (_, _, c) = c

-- | helper function for reading command line.
prompt :: String -> IO String
prompt myString = do
    putStr myString
    hFlush stdout
    getLine

-- | checks if the entered port is a valid port
validPort :: String -> Bool
validPort port =
    all (\x -> any (\y -> x == y) (intArrayToString [0..9])) port || length port == 0

-- | checks if the user wants to create or just join a game.
onlyClient :: String -> Bool
onlyClient "join" = True
onlyClient "JOIN" = True
onlyClient "j" = True
onlyClient "J" = True
onlyClient "create" = False
onlyClient "CREATE" = False
onlyClient "c" = False
onlyClient "C" = False
onlyClient x = False

-- | formats a list of integer to a string
intArrayToString :: [Int] -> String
intArrayToString myArray =
    foldl addToString [] myArray
    where
        addToString myArray element = (head $ show element) : myArray
