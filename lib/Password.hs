{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Password (InputData(..), genPassword, hashMasterPassword) where

import Numeric (showHex)
import Control.Arrow ((>>>))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.Char8
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteArray (ByteArrayAccess)
import Crypto.KDF.PBKDF2 (generate, prfHMAC, Parameters(..), PRF)
import Crypto.Hash.Algorithms (SHA256(..))
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8Lenient)
import Data.Text.Read (hexadecimal)
import Data.String (fromString)

import Config (Config(..))

data InputData = InputData {
  site :: Text,
  login :: Text,
  password :: Text
}

sha256prf :: ByteArrayAccess password => PRF password
sha256prf = prfHMAC SHA256

genPasswordBytes :: InputData -> Config -> ByteString
genPasswordBytes InputData{..} Config{..} =
  generate sha256prf params pass salt
  where
    params = Parameters { iterCounts = 100000, outputLength = 32 }
    pass = encodeUtf8 password
    salt = encodeUtf8 $ T.concat [site, login, T.pack $ showHex cCounter ""]

lowercaseChars, uppercaseChars, digitChars, symbolChars :: String
lowercaseChars = "abcdefghijklmnopqrstuvwxyz"
uppercaseChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
digitChars = "0123456789"
symbolChars = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

ruleFold :: (b -> String -> b) -> b -> Config -> b
ruleFold f accStart Config{..} =
      (\acc -> if cLowercase then f acc lowercaseChars else acc)
  >>> (\acc -> if cUppercase then f acc uppercaseChars else acc)
  >>> (\acc -> if cDigits    then f acc digitChars     else acc)
  >>> (\acc -> if cSymbols   then f acc symbolChars    else acc)
  $   accStart

getSetOfChars :: Config -> ByteString
getSetOfChars = fromString . ruleFold (++) ""

getRulesLength :: Config -> Int
getRulesLength = ruleFold (\b _ -> b + 1) 0

-- The following functions are based on official Python and JS implementations
--  https://github.com/lesspass/lesspass/blob/4280872d3/cli/lesspass/password.py

consumeEntropy :: String -> Integer -> ByteString -> Int -> (String, Integer)
consumeEntropy generatedPwd quotient setOfChars maxLength =
  if length generatedPwd >= maxLength then
    (generatedPwd, quotient)
  else
    let
      (quotient', remainder) = divMod quotient (fromIntegral $ BS.Char8.length setOfChars)
      generatedPwd' = generatedPwd ++ [BS.Char8.index setOfChars (fromInteger remainder)]
    in consumeEntropy generatedPwd' quotient' setOfChars maxLength

getOneCharPerRule :: Integer -> Config -> (String, Integer)
getOneCharPerRule intEntropy = ruleFold f ("", intEntropy)
  where
    f (result, entropy) rule =
      let (value, entropy') = consumeEntropy "" entropy (fromString rule) 1 in
      (result ++ value, entropy')

insertStringPseudoRandomly :: String -> Integer -> String -> String
insertStringPseudoRandomly iPwd intEntropy strToInsert =
  fst $ foldl f (iPwd, intEntropy) strToInsert
  where
    f (pwd, entropy) char =
      let
        (quotient, remainderBig) = divMod entropy (fromIntegral $ length pwd)
        remainder = fromInteger remainderBig :: Int
        pwd' = take remainder pwd ++ (char : drop remainder pwd)
      in (pwd', quotient)

byteStringToHex :: ByteString -> Text
byteStringToHex =
    decodeUtf8Lenient . toStrict . toLazyByteString . byteStringHex

renderPassword :: ByteString -> Config -> String
renderPassword generatedHash cfg =
  let
    entropyHex = byteStringToHex generatedHash
    setOfChars = getSetOfChars cfg
    entropy = case hexadecimal entropyHex of
                Right (n, _) -> n :: Integer
                Left msg -> error msg
    maxLength = cLength cfg - getRulesLength cfg
    (pwd, pwdEntropy) = consumeEntropy "" entropy setOfChars maxLength
    (charsToAdd, charEntropy) = getOneCharPerRule pwdEntropy cfg
  in insertStringPseudoRandomly pwd charEntropy charsToAdd

-- | Generate the result password using the (site, login, masterPassword)
-- triple via the LessPass' algorithm.
genPassword :: InputData -> Config -> String
genPassword input cfg = renderPassword generatedHash cfg
  where generatedHash = genPasswordBytes input cfg

-- | Calculate hash for quickly checking whether the master password is valid
-- or not.
hashMasterPassword :: Text -> ByteString
hashMasterPassword passwordString =
  -- take only the first 24 bits (16 777 216 combinations)
  BS.take 3 $ generate sha256prf params password salt
  where
    params = Parameters { iterCounts = 2, outputLength = 32 }
    password = encodeUtf8 passwordString
    salt = encodeUtf8 ("zbmyeSI3802W6nFEDZJU" :: Text)
