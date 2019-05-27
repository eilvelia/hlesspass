module Getpass (getPassword) where

import System.Posix.Terminal
import System.Posix.IO (stdInput)

-- TODO: Windows

getPasswordTty :: IO String
getPasswordTty = do
  tc <- getTerminalAttributes stdInput
  setTerminalAttributes stdInput (withoutMode tc EnableEcho) Immediately
  password <- getLine
  setTerminalAttributes stdInput tc Immediately
  return password

getPassword :: IO String
getPassword = getPasswordTty <* putStrLn ""
