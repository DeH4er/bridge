{-# LANGUAGE OverloadedStrings #-}

module ParseCommand
  ( Command(..)
  , CommandID(..)
  , parseRequest
  )
where

import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.Types              as AesonTypes
import           Data.Aeson.Types               ( (.:) )
import           Data.ByteString
import           Data.Text


data Command = Connect
             | Echo Text
             deriving (Show)


newtype CommandID = CommandID Int
newtype CommandAction = CommandAction String


parseRequest :: ByteString -> Maybe (Command, CommandID)
parseRequest raw = do
  result               <- Aeson.decodeStrict raw
  (id, action, params) <- flip AesonTypes.parseMaybe result $ \obj -> do
    id     <- obj .: "id"
    action <- obj .: "action"
    params <- obj .: "params"
    return (CommandID id, CommandAction action, params)

  command <- parseCommand action params
  return (command, id)


parseCommand :: CommandAction -> Aeson.Value -> Maybe Command
parseCommand (CommandAction "connect") _ = Just Connect
parseCommand (CommandAction "echo"   ) v = case v of
  (Aeson.String msg) -> Just $ Echo msg
  _                  -> Nothing

parseCommand _ _ = Nothing

