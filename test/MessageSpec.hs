{-# LANGUAGE ScopedTypeVariables #-}
module MessageSpec where
import Message
import Test.Hspec
import Test.QuickCheck
import Data.Text (Text, unpack, pack)

main :: IO ()
main = hspec spec

spec :: Spec 
spec = do
    describe "messageHandler" $ do
        it "input without single quotes" $
            messageHandler (pack "hallo welt") `shouldBe` "{\"command\":\"hallo\",\"parameter\":\"welt\"}"

        it "input with single quotes" $
            messageHandler (pack "hallo 'welt! wie gehts?'") `shouldBe` "{\"command\":\"hallo\",\"parameter\":\"welt! wie gehts?\"}"

        it "input without single quotes and multiple words" $
            messageHandler (pack "hallo welt! wie gehts?") `shouldBe` "{\"command\":\"hallo\",\"parameter\":\"welt!\"}"

        it "input with single quotes and multiple words" $
            messageHandler (pack "hallo 'welt! wie gehts?' alles roger?") `shouldBe` "{\"command\":\"hallo\",\"parameter\":\"welt! wie gehts?\"}"
        
        

        

