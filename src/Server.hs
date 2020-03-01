{-# LANGUAGE OverloadedStrings #-}

module Server where

import qualified Network.Socket                as S
import qualified Network.Socket.ByteString     as SB
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Char8         as C
import           Control.Monad                  ( forever
                                                , when
                                                )
import           Control.Applicative            ( (<$) )
import qualified CommandExecutor
import qualified Data.Aeson                    as Aeson

import           ParseCommand

entry :: IO ()
entry = do
  server <- openServer
  forever $ do
    client <- acceptClient server
    loopClient client

 where
  acceptClient server = do
    (socket, ip) <- S.accept server
    putStrLn $ show ip <> " connected"
    return socket

  openServer = do
    server <- S.socket S.AF_INET S.Stream S.defaultProtocol
    S.bind server (S.SockAddrInet port S.iNADDR_ANY)
    S.listen server 1
    putStrLn $ "Listening on " ++ show port
    return server

  port = 4203


loopClient :: S.Socket -> IO ()
loopClient client = do
  msg <- receiveMessage

  if closedConnection msg
    then do
      putStrLn "Closed connection"
      return ()
    else do
      response <- CommandExecutor.executeRequest msg
      send response
      loopClient client

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
    if isFullMsg msgChunk
      then return fullMsg
      else _receiveMessage fullMsg

  isFullMsg :: BS.ByteString -> Bool
  isFullMsg msg | BS.length msg < maxMsgLen = True
                | C.last msg == '\n'        = True
                | otherwise                 = False

  maxMsgLen = 4096
