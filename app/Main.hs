{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

-- -fno-cse is necessary because of CmdArgs :(

import Control.Monad (unless)
import Data.Functor ((<&>), ($>))
import System.IO (hFlush, stdout)
import System.Directory
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.Char8
import Data.ByteString (ByteString)
import System.Console.CmdArgs

import Config
import Password
import Getpass
import Copy

ver = "0.1.0"
appDirName = ".hlesspass"
configFileName = "config.cfg"
passwordHashFileName = "pwd"

type PasswordHash = ByteString

getAppDir :: IO String
getAppDir = do
  homedir <- getHomeDirectory
  return $ homedir ++ "/" ++ appDirName

initializeApp :: IO (Config, Maybe PasswordHash)
initializeApp = do
  appdir <- getAppDir
  createDirectoryIfMissing False appdir
  let configfile = appdir ++ "/" ++ configFileName
  configExists <- doesFileExist configfile
  config <-
    if configExists
    then readFile configfile <&> parseConfig
    else writeFile configfile configExample $> defaultConfig
  let pwdfile = appdir ++ "/" ++ passwordHashFileName
  pwdfileExists <- doesFileExist pwdfile
  pwd <-
    if pwdfileExists
    then
      BS.readFile pwdfile <&> \content ->
        if BS.null content then Nothing else Just content
    else return Nothing
  return (config, pwd)

writePwdHash :: PasswordHash -> IO ()
writePwdHash bs = do
  appdir <- getAppDir
  BS.writeFile (appdir ++ "/" ++ passwordHashFileName) bs

getMasterPass :: Maybe PasswordHash -> IO String
getMasterPass mhash = do
  putStr "Master password: "
  hFlush stdout
  password <- getPassword
  case mhash of
    Just hash ->
      if hashMasterPass password == hash
        then return password
        else putStrLn "Wrong master password." >> getMasterPass mhash
    Nothing -> return password

start :: Config -> Maybe PasswordHash -> IO ()
start cfg mhash = do
  putStr "Site: "
  hFlush stdout
  site <- getLine
  putStr "Login: "
  hFlush stdout
  login <- getLine
  password <- getMasterPass mhash
  let dat = InputData{..}
  let generated = genPassword dat cfg
  if cCopy cfg
    then copy generated
    else putStrLn generated

data Options = Options
  { oSaveHash :: Bool
  , oNoCheck :: Bool
  , oCopy :: Bool
  , oNoCopy :: Bool
  , oCounter :: Maybe Int
  , oLength :: Maybe Int
  , oLowercase :: Bool
  , oNoLowercase :: Bool
  , oUppercase :: Bool
  , oNoUppercase :: Bool
  , oDigits :: Bool
  , oNoDigits :: Bool
  , oSymbols :: Bool
  , oNoSymbols :: Bool
  }
  deriving (Data, Typeable, Show, Eq)

options = Options
  { oSaveHash = def &= explicit &= name "save-hash"
  , oNoCheck = def &= explicit &= name "no-check"
    &= help "Don't compare hashes"
  , oCopy = def &= explicit &= name "c" &= name "copy"
    &= help "Copy to clipboard"
  , oNoCopy = def &= explicit &= name "no-copy"
    &= help "Don't copy to clipboard"
  , oCounter = def &= explicit &= name "C" &= name "counter"
    &= help "Password counter"
  , oLength = def &= explicit &= name "L" &= name "length"
    &= help "Password length"
  , oLowercase = def &= explicit &= name "l" &= name "lowercase"
  , oNoLowercase = def &= explicit &= name "nl" &= name "no-lowercase"
  , oUppercase = def &= explicit &= name "u" &= name "uppercase"
  , oNoUppercase = def &= explicit &= name "nu" &= name "no-uppercase"
  , oDigits = def &= explicit &= name "d" &= name "digits"
  , oNoDigits = def &= explicit &= name "nd" &= name "no-digits"
  , oSymbols = def &= explicit &= name "s" &= name "symbols"
  , oNoSymbols = def &= explicit &= name "ns" &= name "no-symbols"
  }
  &= program "hlesspass"
  &= summary ("hlesspass - Alternative CLI application for LessPass, v" ++ ver)
  &= helpArg [explicit, name "?", name "h", name "help"]

saveHashMode :: IO ()
saveHashMode = do
  password <- getMasterPass Nothing
  writePwdHash $ hashMasterPass password

optionsIntoConfig :: Options -> Config -> Config
optionsIntoConfig Options{..} =
    (\c -> maybe c (\v -> c { cCounter = v }) oCounter)
  . (\c -> maybe c (\v -> c { cLength = v }) oLength)
  . (\c -> if oCopy then c { cCopy = True } else c)
  . (\c -> if oNoCopy then c { cCopy = False } else c)
  . (\c -> if oLowercase then c { cLowercase = True } else c)
  . (\c -> if oNoLowercase then c { cLowercase = False } else c)
  . (\c -> if oUppercase then c { cUppercase = True } else c)
  . (\c -> if oNoUppercase then c { cUppercase = False } else c)
  . (\c -> if oDigits then c { cDigits = True } else c)
  . (\c -> if oNoDigits then c { cDigits = False } else c)
  . (\c -> if oSymbols then c { cSymbols = True } else c)
  . (\c -> if oNoSymbols then c { cSymbols = False } else c)

main :: IO ()
main = do
  (cfg, mhash) <- initializeApp
  Options{..} <- cmdArgs options
  -- print Options{..}
  if oSaveHash then
    saveHashMode
  else
    let cfg' = optionsIntoConfig Options{..} cfg in
    let mhash' = if oNoCheck then Nothing else mhash in
    start cfg' mhash'
