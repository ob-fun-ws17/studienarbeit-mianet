{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Helper
where

import System.IO

fst' :: (a, b, c) -> a
fst' (a, _, _) = a

snd' :: (a, b, c) -> b
snd' (_, b, _) = b

thd' :: (a, b, c) -> c
thd' (_, _, c) = c

prompt :: String -> IO String
prompt myString = do
    putStr myString
    hFlush stdout
    getLine

validPort :: String -> Bool
validPort port = 
    all (\x -> any (\y -> x == y) (intArrayToString [0..9])) port || length port == 0

intArrayToString :: [Int] -> String
intArrayToString myArray =
    foldl addToString [] myArray
    where 
        addToString myArray element = (head $ show element) : myArray