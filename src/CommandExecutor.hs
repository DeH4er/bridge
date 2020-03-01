{-# LANGUAGE OverloadedStrings #-}

module CommandExecutor where

import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import ParseCommand

executeRequest :: BS.ByteString -> IO BS.ByteString
executeRequest requestRaw = do
  let request = parseRequest requestRaw
  case request of
    Just (command, CommandID id) -> do
      response <- CommandExecutor.executeCommand command
      return $ encode response
    Nothing -> return "Unknown command"

  where
    encode :: Maybe Aeson.Value -> BS.ByteString
    encode response = case response of
      Just v -> toStrict . Aeson.encode $ v
      Nothing -> ""

    toStrict :: BL.ByteString -> BS.ByteString
    toStrict = BS.concat . BL.toChunks

executeCommand :: Command -> IO (Maybe Aeson.Value)
executeCommand Connect = do
  putStrLn "Connect command executed"
  return Nothing

executeCommand (Echo msg) = do
  putStrLn "Echo command executed. Return msg back"
  return $ Just $ Aeson.String msg
