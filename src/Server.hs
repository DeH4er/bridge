{-# LANGUAGE OverloadedStrings #-}

module Server where

import qualified Data.Aeson                    as Aeson
import qualified Control.Exception             as E
import qualified Data.ByteString               as S
import qualified Network.Socket.ByteString     as SB
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as C

import           Control.Concurrent             ( forkFinally )
import           Control.Monad                  ( forever
                                                , when
                                                , unless
                                                , void
                                                )
import           Network.Socket

import qualified CommandExecutor
import           ParseCommand




entry :: IO ()
entry = runTCPServer Nothing "4203" loopClient


loopClient :: Socket -> IO ()
loopClient client = do
  request <- receiveMessage

  if not $ closedConnection request
    then do
      response <- CommandExecutor.executeRequest request
      send response
      loopClient client
    else putStrLn "Closed connection"

 where
  receiveMessage :: IO BS.ByteString
  receiveMessage = SB.recv client maxMsgLen

  closedConnection :: BS.ByteString -> Bool
  closedConnection "" = True
  closedConnection _  = False

  send :: BS.ByteString -> IO ()
  send msg = do
    let msgLen = BS.length msg
    bytesSent <- SB.send client msg
    when (bytesSent < msgLen) $ send $ BS.drop bytesSent msg

  _receiveMessage :: BS.ByteString -> IO BS.ByteString
  _receiveMessage msg = do
    msgChunk <- SB.recv client maxMsgLen
    let fullMsg = msgChunk <> msg
    if isFullMsg msgChunk then return fullMsg else _receiveMessage fullMsg

  isFullMsg :: BS.ByteString -> Bool
  isFullMsg msg | BS.length msg < maxMsgLen = True
                | C.last msg == '\n'        = True
                | otherwise                 = False

  maxMsgLen = 4096


runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close loop
 where
  resolve = head <$> getAddrInfo (Just hints) mhost (Just port)
   where
    hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }

  open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption sock ReuseAddr 1
    withFdSocket sock setCloseOnExecIfNeeded
    bind sock $ addrAddress addr
    listen sock 1024
    return sock

  loop sock = forever $ do
    (conn, _peer) <- accept sock
    void $ forkFinally (server conn) (const $ gracefulClose conn 5000)

