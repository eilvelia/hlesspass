{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Password
import Config

main :: IO ()
main = passwordSpec

passwordSpec :: IO ()
passwordSpec = hspec $ describe "Password generation" $ do
  let dat1 = InputData {
    site = "example.org",
    login = "contact@example.org",
    password = "password"
  }
  let dat2 = InputData {
    site = "",
    login = "b",
    password = "c"
  }
  it "default options" $
    genPassword dat1 defaultConfig
      `shouldBe` "WHLpUL)e00[iHR+w"
  it "without symbols" $
    genPassword dat1 defaultConfig { cSymbols = False }
      `shouldBe` "Jyd57m7Ctw2695ks"
  it "without symbols and length=24" $
    genPassword dat1 defaultConfig { cSymbols = False, cLength = 24 }
      `shouldBe` "y5m7Ctw2696F5ksih1ZFmz3b"
  it "with length=12 and counter=14" $
    genPassword dat1 defaultConfig { cLength = 12, cCounter = 14 }
      `shouldBe` "k*EnKS\\9_wJm"
  it "with empty 'site'" $
    genPassword dat2 defaultConfig
      `shouldBe` "F)vWZa{GjMk`=5=T"
  it "with unicode characters" $
    genPassword (dat1 { password = "🧡 привет" }) defaultConfig
      `shouldBe` "m/8}II450c9Ei+cZ"
