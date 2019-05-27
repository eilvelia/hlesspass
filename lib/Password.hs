{-# LANGUAGE RecordWildCards #-}

module Password where

import Config (Config(..))

import Numeric (showHex, readHex)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Lazy.Char8 as BSL.Char8
import Data.ByteString.Builder (toLazyByteString, byteStringHex)
import Data.ByteArray (ByteArrayAccess, Bytes(..))
import Crypto.KDF.PBKDF2
import Crypto.Hash.Algorithms (SHA256(..))

-- Fast check if master password is valid
hashMasterPass :: String -> ByteString
hashMasterPass pwdStr =
  let appsalt = "zbmyeSI3802W6nFEDZJU" in
  let p = Parameters { iterCounts = 2, outputLength = 32 } in
  let pwd = BS.Char8.pack pwdStr in
  let hash = generate sha256prf p pwd (BS.Char8.pack appsalt) in
  BS.take 3 hash -- take only 3 bytes, 2^24 (16 777 216) combinations

data InputData = InputData {
  site :: String,
  login :: String,
  password :: String
}

sha256prf :: ByteArrayAccess password => PRF password
sha256prf = prfHMAC SHA256

params = Parameters { iterCounts = 100000, outputLength = 32 }

genPasswordBytes :: InputData -> Config -> ByteString
genPasswordBytes InputData{..} Config{..} =
  let pwd = BS.Char8.pack password in
  let salt = BS.Char8.pack $ site ++ login ++ showHex cCounter "" in
  generate sha256prf params pwd salt

lowercaseChars = "abcdefghijklmnopqrstuvwxyz"
uppercaseChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
digitChars = "0123456789"
symbolChars = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

ruleFold :: (b -> String -> b) -> b -> Config -> b
ruleFold f acc0 Config{..} =
  acc4
  where
    acc1 = if cLowercase then f acc0 lowercaseChars else acc0
    acc2 = if cUppercase then f acc1 uppercaseChars else acc1
    acc3 = if cDigits then f acc2 digitChars else acc2
    acc4 = if cSymbols then f acc3 symbolChars else acc3

getSetOfChars :: Config -> String
getSetOfChars = ruleFold (++) ""

getRulesLength :: Config -> Int
getRulesLength = ruleFold (\b _ -> b + 1) 0

-- FYI these functions below are inefficient

-- Based on official Python and JS implementations
--  https://github.com/lesspass/lesspass/blob/4280872d3/cli/lesspass/password.py

-- TODO: Doesn't work properly with Unicode symbols

consumeEntropy :: String -> Integer -> String -> Int -> (String, Integer)
consumeEntropy generatedPwd quotient setOfChars maxLength =
  if length generatedPwd >= maxLength then
    (generatedPwd, quotient)
  else
    let (quotient', remainder) = divMod quotient (fromIntegral $ length setOfChars) in
    let generatedPwd' = generatedPwd ++ [setOfChars !! fromInteger remainder] in
    consumeEntropy generatedPwd' quotient' setOfChars maxLength

getOneCharPerRule :: Integer -> Config -> (String, Integer)
getOneCharPerRule iEntropy =
  ruleFold f ("", iEntropy)
  where
    f (result, entropy) str =
      let (value, entropy') = consumeEntropy "" entropy str 1 in
      (result ++ value, entropy')

insertStringPseudoRandomly :: String -> Integer -> String -> String
insertStringPseudoRandomly iPwd iEntropy strToInsert =
  fst $ foldl f (iPwd, iEntropy) strToInsert
  where
    f (pwd, entropy) char =
      let (quotient, remainderBig) = divMod entropy (fromIntegral $ length pwd) in
      let remainder = fromInteger remainderBig :: Int in
      let pwd' = take remainder pwd ++ (char : drop remainder pwd) in
      (pwd', quotient)

bsToHexStr :: ByteString -> String
bsToHexStr = BSL.Char8.unpack . toLazyByteString . byteStringHex

renderPassword :: String -> Config -> String
renderPassword entropyHex cfg =
  let setOfChars = getSetOfChars cfg in
  let entropy = case readHex entropyHex :: [(Integer, String)] of
        [(n, v)] -> n
        _ -> error $ "invalid hex (" ++ entropyHex ++ ")"
  in
  let maxLength = cLength cfg - getRulesLength cfg in
  let (pwd, pwdEntropy) = consumeEntropy "" entropy setOfChars maxLength in
  let (charsToAdd, charEntropy) = getOneCharPerRule pwdEntropy cfg in
  insertStringPseudoRandomly pwd charEntropy charsToAdd

genPassword :: InputData -> Config -> String
genPassword dat cfg = renderPassword (bsToHexStr $ genPasswordBytes dat cfg) cfg
