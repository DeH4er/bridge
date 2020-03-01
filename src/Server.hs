{-# LANGUAGE OverloadedStrings #-}

module Server where

import qualified Network.Socket                as S
import qualified Network.Socket.ByteString     as SB
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Char8         as C
import           Control.Monad                  ( forever )
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
  acceptClient server = fst <$> S.accept server

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
    then return ()
    else do
      response <- CommandExecutor.executeRequest msg
      send client response

      loopClient client

 where
  receiveMessage :: IO BS.ByteString
  receiveMessage = SB.recv client maxMsgLen

  closedConnection :: BS.ByteString -> Bool
  closedConnection "" = True
  closedConnection _  = False

  send :: S.Socket -> BS.ByteString -> IO ()
  send client msg = do
    let msgLen = BS.length msg
    bytesSent <- SB.send client msg
    if bytesSent < msgLen
      then send client $ BS.drop bytesSent msg
      else return ()

  _receiveMessage :: BS.ByteString -> IO BS.ByteString
  _receiveMessage msg = do
    msgChunk <- SB.recv client maxMsgLen
    let len = BS.length msgChunk
    if isFullMsg msgChunk len
      then return $ msgChunk <> msg
      else _receiveMessage $ msgChunk <> msg

  isFullMsg :: BS.ByteString -> Int -> Bool
  isFullMsg msg len | len < maxMsgLen    = True
                    | C.last msg == '\n' = True
                    | otherwise          = False

  maxMsgLen = 4096
