{-# LANGUAGE ScopedTypeVariables #-}
module DiceSpec where
import Dice
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec 
spec = do
    describe "formatNums" $ do
        it "double formating (num1 == num2): formatNums 3 3 --> Result: 330" $
            formatNums 3 3 `shouldBe` 330
        it "normal dice result formating (num1 > num2): formatNums 5 1 --> Result: 51" $
            formatNums 5 1 `shouldBe` 51
        it "normal dice result formating (num1 < num2): formatNums 1 5 --> Result: 51" $
            formatNums 1 5 `shouldBe` 51
        it "highest possible score formating (num1 == 1 and num2 == 2): formatNums 1 2 --> Result: 2100" $
            formatNums 1 2 `shouldBe` 2100
        it "highest possible score formating (num1 == 2 and num2 == 1): formatNums 2 1 --> Result: 2100" $
            formatNums 2 1 `shouldBe` 2100

    describe "deformatNums" $ do
        it "deformat the highest possible score (num = 2100) --> Result 21" $
            deformatNums 2100 `shouldBe` 21
        it "deformat a double (num > 100)" $
            deformatNums 120 `shouldBe` 12
        it "deformat a normal diced result (num = 32)" $
            deformatNums 32 `shouldBe` 32

    -- describe "tossDice" $ do
    --     it "toss the Dice randomly" $ do
    --         g <- newStdGen
    --         tossDice g `shouldBe` tossDice g
