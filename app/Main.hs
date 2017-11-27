module Main where

import Lib (tossDice)
import System.Random
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
 hSetBuffering stdout NoBuffering
 putStr "Welcome to MiaNet - The haskell online version of the game Mia (quit == q): "
 putStrLn ("tossDice() = ")
 text <- readLn :: IO String
 g <- newStdGen
 f <- newStdGen
 print $ ((tossDice g), (tossDice f))
 if text == "q"
  then putStrLn "Ciao"
  else main
