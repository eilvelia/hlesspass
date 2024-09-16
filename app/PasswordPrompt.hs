module PasswordPrompt (promptPassword) where

import System.Posix.Terminal
    ( TerminalState(..),
      TerminalMode(..),
      getTerminalAttributes,
      setTerminalAttributes,
      withoutMode )
import System.Posix.IO (stdInput)
import Data.Text (Text)
import qualified Data.Text.IO as T.IO

-- TODO: Windows

promptPasswordTty :: IO Text
promptPasswordTty = do
  tc <- getTerminalAttributes stdInput
  setTerminalAttributes stdInput (withoutMode tc EnableEcho) Immediately
  password <- T.IO.getLine
  setTerminalAttributes stdInput tc Immediately
  putStrLn ""
  return password

promptPassword :: IO Text
promptPassword = promptPasswordTty
