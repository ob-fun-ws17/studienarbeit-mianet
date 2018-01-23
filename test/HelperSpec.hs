{-# LANGUAGE ScopedTypeVariables #-}
module HelperSpec (spec) where

import Dice (tossDice)
import System.Random
import Data.Char
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
    describe "tossDice" $ do
        it "toss the Dice randomly" $ do
            g <- newStdGen
            tossDice g `shouldBe` tossDice g
