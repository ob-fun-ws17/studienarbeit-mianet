module Main where

import Lib (tossDice, runUDPServer, sendMessage)
import Network.Socket
import Network.Multicast
import Control.Concurrent        (forkIO, threadDelay)


main :: IO ()
main = do
  _ <- forkIO $ runUDPServer
  threadDelay 1000000 -- wait one second

-- hSetBuffering stdout NoBuffering
-- putStr "Welcome to MiaNet - The haskell online version of the game Mia (quit == q): "
-- putStrLn ("tossDice() = ")
-- text <- readLn :: IO String
 --g <- newStdGen
 --f <- newStdGen
 -- print $ ((tossDice g), (tossDice f))
-- if text == "q"
--  then putStrLn "Ciao"
--  else main
