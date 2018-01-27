{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{- |
Module      : Dice
Description : Dice module.
Copyright   : 2018 Philipp Mayer & Engelbrecht Nils
License     : BSD3
Maintainer  : Philipp Mayer, Nils Engelbrecht
-}
module Dice where

-------------------------------------------------------------
import System.Random
-------------------------------------------------------------

-- | string represents the dice.
dice :: String
dice = "123456"

-- | tossDice definition takes RandomGen and returns String.
tossDice :: RandomGen g => g -> [Char]
tossDice g = take 1 (randomRs ('1', '6') g)

-- | rolls a 6-sided-dice and returns the result.
rollDice :: IO Int
rollDice = getStdRandom (randomR (1,6))

-- | takes two numbers (the rolled dices), puts them together and muliplicates it with a factor.
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

-- | reverse function of formatNums.
deformatNums :: Int -> Int
deformatNums myNum =
    case (myNum) of
      _ | (myNum == 2100) ->
            myNum `div` 100

        | (myNum > 100) ->
            myNum `div` 10

        | otherwise ->
            myNum
