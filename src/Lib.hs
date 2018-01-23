-- | A Lib module.
module Lib
    ( tossDice
    , broadcastGameInfo
    , receiveGameInfo
    ) where

import System.Random
import System.Environment
import Control.Concurrent        (forkIO, threadDelay)
import Control.Monad             (forever)
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding     (recv)
import Network.Socket.ByteString (recv, sendAll)

tossDice :: RandomGen g => g -> [Char]
tossDice g = take 1 (randomRs ('1', '6') g)

broadcastGameInfo :: String -> IO ()
broadcastGameInfo s = do
    addrinfos <- getAddrInfo Nothing (Just "255.255.255.255") (Just "9990")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    setSocketOption sock Broadcast 1
    connect sock (addrAddress serveraddr)
    sendAll sock $ C.pack s
    close sock
    threadDelay 1000000

receiveGameInfo :: IO ()
receiveGameInfo = do
  addrinfos <- getAddrInfo Nothing (Just "0.0.0.0") (Just "9990")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  bind sock (addrAddress serveraddr)
  --forever (do recv sock 8096 >>= print)
  print "Looking for games..."
  recv sock 8096 >>= \message -> print (C.unpack message)
  close sock
