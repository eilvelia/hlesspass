module Config (configExample, Config(..), defaultConfig, parseConfig) where

import Data.Bifunctor (bimap)

-- inefficient
strip, lstrip, rstrip :: String -> String
strip = lstrip . rstrip
lstrip = dropWhile (`elem` " \t\r\n")
rstrip = reverse . lstrip . reverse

configExample :: String
configExample = unlines [
  "lowercase=yes",
  "uppercase=yes",
  "digits=yes",
  "symbols=yes",
  "length=16",
  "counter=1",
  "copy=no"
  ]

data Config = Config {
  cLowercase :: Bool,
  cUppercase :: Bool,
  cDigits :: Bool,
  cSymbols :: Bool,
  cLength :: Int,
  cCounter :: Int,
  cCopy :: Bool
} deriving (Eq, Show)

defaultConfig = Config {
  cLowercase = True,
  cUppercase = True,
  cDigits = True,
  cSymbols = True,
  cLength = 16,
  cCounter = 1,
  cCopy = False
}

parseBool :: String -> Bool
parseBool "yes" = True
parseBool _ = False

parseInt :: String -> Int
parseInt str =
  case reads str :: [(Int, String)] of
    [(x, _)] -> x
    _ -> error $ "Invalid int (" ++ str ++ ")"

parseNewField :: Config -> String -> Config
parseNewField old l | l' == "" || elem (head l') "#;" = old where l' = strip l
parseNewField old line =
  case fieldName of
    "lowercase" -> old { cLowercase = parseBool v }
    "uppercase" -> old { cUppercase = parseBool v }
    "digits" -> old { cDigits = parseBool v }
    "symbols" -> old { cSymbols = parseBool v }
    "length" -> old { cLength = parseInt v }
    "counter" -> old { cCounter = parseInt v }
    "copy" -> old { cCopy = parseBool v }
    _ -> error $ "Invalid config line: " ++ line
  where
    (fieldName, v) = bimap strip (strip . tail) $ span (/= '=') line

parseConfig :: String -> Config
parseConfig str = foldl parseNewField defaultConfig (lines str)
