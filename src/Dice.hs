{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Dice
where

import System.Random
-------------------------------------------------------------

dice :: String
dice = "123456"

rollDice :: IO Int
rollDice = getStdRandom (randomR (1,6)) 

formatNums :: Int -> Int -> Int
formatNums num1 num2 =
    case (num1, num2) of
      _ | (num1 == 1 && num2 == 2) ->
            100 * (10 * num2 + num1)

        | (num1 == 2 && num2 == 1) ->
            100 * (10 * num1 + num2)
        
        | (num1 > num2) ->
            (10 * num1 + num2)
        
        | (num1 < num2) ->
            (10 * num2 + num1)

        | (num1 == num2) ->
            10 * (10 * num1 + num2) 

deformatNums :: Int -> Int
deformatNums myNum =
    case (myNum) of
      _ | (myNum == 2100) ->
            myNum `div` 100

        | (myNum > 100) ->
            myNum `div` 10
        
        | otherwise ->
            myNum