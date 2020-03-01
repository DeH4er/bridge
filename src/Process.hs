{-# LANGUAGE OverloadedStrings #-}

module Process where
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import CommandExecutor
import Control.Monad (forever)

entry :: IO ()
entry = forever $ do
  request <- BS.getLine
  response <- executeRequest request
  C.putStrLn response

