{-# LANGUAGE RecordWildCards #-}

module Config (Config(..), defaultConfig, parseConfig) where

import Data.Bifunctor (second)

data Config = Config {
  cLowercase :: Bool,
  cUppercase :: Bool,
  cDigits :: Bool,
  cSymbols :: Bool,
  cLength :: Int,
  cCounter :: Int,
  cCopy :: Bool
} deriving (Eq)

instance Show Config where
  show Config{..} =
    unlines [
      "lowercase=" ++ showBool cLowercase,
      "uppercase=" ++ showBool cUppercase,
      "digits=" ++ showBool cDigits,
      "symbols=" ++ showBool cSymbols,
      "length=" ++ showInt cLength,
      "counter=" ++ showInt cCounter,
      "copy=" ++ showBool cCopy
    ]
    where
      showBool True = "yes"
      showBool False = "no"
      showInt = show :: Int -> String

defaultConfig :: Config
defaultConfig = Config {
  cLowercase = True,
  cUppercase = True,
  cDigits = True,
  cSymbols = True,
  cLength = 16,
  cCounter = 1,
  cCopy = False
}

parseBoolValue :: String -> Bool
parseBoolValue "yes" = True
parseBoolValue "true" = True
parseBoolValue "no" = False
parseBoolValue "false" = False
parseBoolValue "" = False
parseBoolValue _ = error "Invalid bool value"

parseIntValue :: String -> Int
parseIntValue str =
  case reads str :: [(Int, String)] of
    [(x, _)] -> x
    _ -> error $ "Invalid int (" ++ str ++ ")"

parseField :: Config -> String -> Config
parseField old line = if head key `elem` "#;" then old else forKey key
  where
    (key, value) = second tail . span (/= '=') . filter (/= ' ') $ line
    forKey "lowercase" = old { cLowercase = parseBoolValue value }
    forKey "uppercase" = old { cUppercase = parseBoolValue value }
    forKey "digits" = old { cDigits = parseBoolValue value }
    forKey "symbols" = old { cSymbols = parseBoolValue value }
    forKey "length" = old { cLength = parseIntValue value }
    forKey "counter" = old { cCounter = parseIntValue value }
    forKey "copy" = old { cCopy = parseBoolValue value }
    forKey _ = error $ "Invalid config line: " ++ line

parseConfig :: String -> Config
parseConfig str = foldl parseField defaultConfig (lines str)
