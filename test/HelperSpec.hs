{-# LANGUAGE ScopedTypeVariables #-}
module HelperSpec where
import Helper
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec 
spec = do
    describe "3er Tupel (a, b, c)" $ do
        it "first element of a 3-element Tupel" $
            fst' (1, 2, 3) `shouldBe` 1
        it "second element of a 3-element Tupel" $
            snd' (1, 2, 3) `shouldBe` 2

        it "third element of a 3-element Tupel" $
            thd' (1, 2, 3) `shouldBe` 3

    describe "validPort" $ do
        it "it's a valid port: 9000" $
            validPort "9000" `shouldBe` True

        it "it isn't a valid port: knäckebrot" $
            validPort "knäckebrot" `shouldBe` False

    describe "list of integer to String" $ do
        it "dice list to string" $
            intArrayToString [1..6] `shouldBe` "654321"

        

