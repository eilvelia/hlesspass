{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- -fno-cse is necessary because of CmdArgs :(
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Data.Functor ((<&>))
import System.IO (hFlush, stdout)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Environment.XDG.BaseDir (getUserConfigDir)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Text (Text)
import System.Console.CmdArgs.Implicit

import Config (Config(..), defaultConfig, parseConfig)
import Password (InputData(..), genPassword, hashMasterPassword)
import PasswordPrompt (promptPassword)
import Copy (copy)

appName, appVersion, configFileName, passwordHashFileName :: String
appName = "hlesspass"
appVersion = "0.2.0"
configFileName = "config.cfg"
passwordHashFileName = "pwd"

type PasswordHash = ByteString

readAppDir :: IO (Config, Maybe PasswordHash)
readAppDir = do
  appConfigDir <- getUserConfigDir appName
  createDirectoryIfMissing True appConfigDir
  savedConfig <- getConfig (appConfigDir </> configFileName)
  savedPwdHash <- getMasterPasswordHash (appConfigDir </> passwordHashFileName)
  return (savedConfig, savedPwdHash)
  where
    getConfig file =
      doesFileExist file >>= \case
        True -> parseConfig <$> readFile file
        -- Write the default config if not existent
        False -> defaultConfig <$ writeFile file (show defaultConfig)
    getMasterPasswordHash file =
      doesFileExist file >>= \case
        True -> BS.readFile file <&> \content ->
          if BS.null content then Nothing else Just content
        False -> return Nothing

writePwdHash :: PasswordHash -> IO ()
writePwdHash hashByteString = do
  appConfigDir <- getUserConfigDir appName
  createDirectoryIfMissing True appConfigDir
  BS.writeFile (appConfigDir </> passwordHashFileName) hashByteString

prompt :: Text -> IO Text
prompt question = T.IO.putStr question >> hFlush stdout >> T.IO.getLine

getSite :: Bool -> IO Text
getSite True = lookupEnv "HLESSPASS_SITE" >>= \case
  Just site -> return $ T.pack site
  Nothing -> prompt "Site: "
getSite False = prompt "Site: "

getLogin :: Bool -> IO Text
getLogin True = lookupEnv "HLESSPASS_LOGIN" >>= \case
  Just login -> return $ T.pack login
  Nothing -> prompt "Login: "
getLogin False = prompt "Login: "

getMasterPassword :: Maybe PasswordHash -> Bool -> IO Text
getMasterPassword savedPwdHash useEnv
  | useEnv = lookupEnv "HLESSPASS_PASSWORD" >>= \case
    Just masterPassword -> return $ T.pack masterPassword
    Nothing -> promptMasterPassword
  | otherwise = promptMasterPassword
  where
    promptMasterPassword = do
      putStr "Master password: "
      hFlush stdout
      password <- promptPassword
      case savedPwdHash of
        Just hash | hashMasterPassword password == hash -> return password
        Just _ -> putStrLn "Wrong master password." >> promptMasterPassword
        Nothing -> return password

start :: Config -> Maybe PasswordHash -> Bool -> IO ()
start cfg savedPwdHash useEnv = do
  site <- getSite useEnv
  login <- getLogin useEnv
  password <- getMasterPassword savedPwdHash useEnv
  let generated = genPassword InputData{..} cfg
  if cCopy cfg then copy generated else putStrLn generated

saveHashMode :: IO ()
saveHashMode = do
  password <- getMasterPassword Nothing False
  writePwdHash $ hashMasterPassword password

data CliOptions = CliOptions
  { oSaveHash :: Bool
  , oNoCheck :: Bool
  , oEnv :: Bool
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

options :: CliOptions
options = CliOptions
  { oSaveHash = def &= explicit &= name "save-hash"
  , oNoCheck = def &= explicit &= name "no-check"
    &= help "Don't compare hashes"
  , oEnv = def &= explicit &= name "env"
    &= help "Read the HLESSPASS_{SITE,LOGIN,PASSWORD} env variables"
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
  &= summary ("hlesspass - Alternative CLI application for LessPass, v" ++ appVersion)
  &= helpArg [explicit, name "?", name "h", name "help"]
  &= versionArg [explicit, name "version"]

optionsIntoConfig :: CliOptions -> Config -> Config
optionsIntoConfig CliOptions{..} =
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
  (savedConfig, savedPwdHash) <- readAppDir
  CliOptions{..} <- cmdArgs options
  if oSaveHash then
    saveHashMode
  else do
    let config = optionsIntoConfig CliOptions{..} savedConfig
    start config (if oNoCheck then Nothing else savedPwdHash) oEnv
