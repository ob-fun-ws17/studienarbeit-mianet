-- | A Lib module.
module Lib
    ( broadcastGameInfo
    , receiveGameInfo
    ) where

import System.Environment
import System.Timeout
import Control.Concurrent        (forkIO, threadDelay, forkOS)
import Control.Monad             (forever)
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding     (recv)
import Network.Socket.ByteString (recv, sendAll)
import Data.Maybe

-- | broadcast relevant Information ( hostname and port ).
broadcastGameInfo :: String -> IO ()
broadcastGameInfo s = do
    sock <- socket AF_INET Datagram defaultProtocol
    setSocketOption sock Broadcast 1
    connect sock (SockAddrInet 9559 (tupleToHostAddress (255,255,255,255)))
    sendAll sock $ C.pack s
    close sock
    threadDelay 1000000

-- | receive broadcasting information in order to connect to a game.
receiveGameInfo :: IO (Maybe C.ByteString)
receiveGameInfo = do
  sock <- socket AF_INET Datagram defaultProtocol    -- create socket
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 9559 iNADDR_ANY)
  msg <- timeout 5000000 $ recv sock 8096
  close sock
  return msg
