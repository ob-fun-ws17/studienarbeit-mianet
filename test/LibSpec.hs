{-# LANGUAGE ScopedTypeVariables #-}
module LibSpec (spec) where

import Lib (palindrom)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
    describe "palindrom" $ do
        it "is 'test' a palindrom" $
            palindrom "test" `shouldBe` False
        it "is 'AnnA' a palindrom" $
            palindrom "AnnA" `shouldBe` True
        it "is 'Anna' a palindrom" $
            palindrom "Anna" `shouldBe` True
        it "is 'A nna' a palindrom" $
            palindrom "A nna" `shouldBe` False
        it "is 'An na' a palindrom" $
            palindrom "An na" `shouldBe` False
        it "is 'An nA' a palindrom" $
            palindrom "An nA" `shouldBe` True