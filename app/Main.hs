module Main where

import qualified System.Environment            as Env
import qualified Server
import qualified Process


entry :: IO ()
entry = do
  args <- Env.getArgs
  case args of
    [arg] -> runArg $ strToArg arg
    _     -> invalidCommand


data Arg = Server
         | Process
         | Help
         | InvalidArg


strToArg :: String -> Arg
strToArg "server"  = Server
strToArg "process" = Process
strToArg "help"    = Help
strToArg _         = InvalidArg


runArg :: Arg -> IO ()
runArg Server  = serverCommand
runArg Process = processCommand
runArg Help    = helpCommand
runArg InvalidArg  = invalidCommand


processCommand :: IO ()
processCommand = Process.entry


serverCommand :: IO ()
serverCommand = Server.entry


helpCommand :: IO ()
helpCommand = putStrLn $
  "Available commands: \n" ++
  "process - runs the bridge with communication via stdin/stdout\n" ++
  "server - runs the bridge with communication via socket."


invalidCommand :: IO ()
invalidCommand = do
  putStrLn "Invalid command."
  h
