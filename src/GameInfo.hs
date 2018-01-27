{- |
Module      : GameInfo
Description : module used for broadcasting.
Copyright   : 2018 Philipp Mayer & Engelbrecht Nils
License     : BSD3
Maintainer  : Nils Engelbrecht
-}
module GameInfo
    ( broadcastGameInfo
    , receiveGameInfo
    , returnHostPortTupel
    ) where
-- This module is used for broadcasting and receiving game info.

import           System.Environment
import           System.Timeout
import           Control.Concurrent        (forkIO, threadDelay, forkOS)
import           Control.Monad             (forever)
import qualified Data.ByteString.Char8     as C
import           Network.Socket            hiding (recv)
import           Network.Socket.ByteString (recv, sendAll)
import           Data.List.Split
import           Data.Maybe

-- | broadcasts relevant information ( hostname and port ) to 255.255.255.255 on port 9559.
broadcastGameInfo :: String -> IO ()
broadcastGameInfo s = do
    sock <- socket AF_INET Datagram defaultProtocol
    setSocketOption sock Broadcast 1
    connect sock (SockAddrInet 9559 (tupleToHostAddress (255,255,255,255)))
    sendAll sock $ C.pack s
    close sock
    threadDelay 1000000

-- | receives broadcasting information on port 9559 on 0.0.0.0 in order to connect to a game.
receiveGameInfo :: IO (Maybe C.ByteString)
receiveGameInfo = do
  sock <- socket AF_INET Datagram defaultProtocol    -- create socket
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 9559 iNADDR_ANY)
  msg <- timeout 5000000 $ recv sock 8096
  close sock
  return msg

-- | method to get hostname and port in a tupel out of one string.
returnHostPortTupel :: String -> (String, Int)
returnHostPortTupel x = (head $ splitOn ":" x, read (last $ splitOn ":" x))
