{-# LANGUAGE ScopedTypeVariables #-}
module GameInfoSpec
  (spec) where

import GameInfo
import System.Random
import Data.Char
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
    describe "hostPortTupel" $ do
        it "check host of GameInfoString" $ do
            let gameInfoString = "127.0.0.1:8080"
            let tupel = returnHostPortTupel gameInfoString
            fst tupel `shouldBe` "127.0.0.1"
        it "check port of GameInfoString" $ do
            let gameInfoString = "127.0.0.1:8080"
            let tupel = returnHostPortTupel gameInfoString
            snd tupel `shouldBe` 8080
