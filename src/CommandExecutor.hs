{-# LANGUAGE OverloadedStrings #-}

module CommandExecutor where

import qualified Data.Aeson                    as Aeson
import           Data.Aeson                     ( (.=) )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy          as BL
import           ParseCommand

executeRequest :: BS.ByteString -> IO BS.ByteString
executeRequest requestRaw = do
  let request = parseRequest requestRaw
  case request of
    Just (command, id) -> do
      putStrLn $ "Execute " <> show command
      response <- CommandExecutor.executeCommand command
      return $ encode response id
    Nothing -> do
      C.putStrLn $ "Invalid command format of " <> requestRaw
      return "Invalid command format"

 where
  encode :: Aeson.Value -> CommandID -> BS.ByteString
  encode response (CommandID id) =
    toStrict . Aeson.encode $ Aeson.object ["result" .= response, "id" .= id]

  toStrict :: BL.ByteString -> BS.ByteString
  toStrict = BS.concat . BL.toChunks

executeCommand :: Command -> IO Aeson.Value
executeCommand Connect = do
  putStrLn "Connect command executed"
  return Aeson.Null

executeCommand (Echo msg) = do
  putStrLn "Echo command executed. Return msg back"
  return $ Aeson.String msg
