module Lib where

import qualified System.Environment            as Env
import qualified Server
import qualified Process


entry :: IO ()
entry = do
  args <- Env.getArgs
  case args of
    [arg] -> runArg $ strToArg arg
    _     -> invalidCommand


data Arg = ServerArg
         | ProcessArg
         | HelpArg
         | InvalidArg


strToArg :: String -> Arg
strToArg "server"  = ServerArg
strToArg "process" = ProcessArg
strToArg "help"    = HelpArg
strToArg _         = InvalidArg


runArg :: Arg -> IO ()
runArg ServerArg  = serverCommand
runArg ProcessArg = processCommand
runArg HelpArg    = helpCommand
runArg InvalidArg = invalidCommand


processCommand = Process.entry
processCommand :: IO ()


serverCommand :: IO ()
serverCommand = Server.entry


helpCommand :: IO ()
helpCommand =
  putStrLn
    $  "Available commands: \n"
    <> "process - runs the bridge with communication via stdin/stdout\n"
    <> "server - runs the bridge with communication via socket.\n"
    <> "help - show this message."


invalidCommand :: IO ()
invalidCommand = do
  putStrLn "Invalid command.\n"
  helpCommand
