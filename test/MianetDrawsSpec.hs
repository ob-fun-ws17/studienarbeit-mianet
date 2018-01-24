{-# LANGUAGE ScopedTypeVariables #-}
module MianetDrawsSpec where
import MianetDraws
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec 
spec = do
    describe "validResult" $ do
        it "unvalid result" $
            validResult "turnschuh" `shouldBe` False
        it "valid result" $
            validResult "22" `shouldBe` True

    describe "addToListWithConv" $ do
        it "valid char -> just []" $
            addToListWithConv' [1,2,3] '4' `shouldBe` Just [4,1,2,3]

        it "unvalid char -> nothing" $
            addToListWithConv' [1,2,3] 'b' `shouldBe` Nothing
        

        

