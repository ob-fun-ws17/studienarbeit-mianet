{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Rules
(rollDice
) where

import System.Random
-------------------------------------------------------------

rollDice :: IO Int
rollDice = getStdRandom (randomR (1,6)) 



