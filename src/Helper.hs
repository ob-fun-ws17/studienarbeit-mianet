{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
-- | A Helper module.
module Helper
where

import System.IO

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

-- | formats a list of integer to a string
intArrayToString :: [Int] -> String
intArrayToString myArray =
    foldl addToString [] myArray
    where
        addToString myArray element = (head $ show element) : myArray
