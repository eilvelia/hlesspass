import Test.Hspec
import Password
import Config

main :: IO ()
main = passwordSpec

fullCharset :: String
fullCharset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789\
\!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

passwordSpec :: IO ()
passwordSpec = hspec $ describe "Password" $ do
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
  it "consumeEntropy" $
    consumeEntropy "" (4 * 4 + 2) "abcd" 2 `shouldBe` ("ca", 1)
  it "getSetOfChars" $
    getSetOfChars defaultConfig `shouldBe` fullCharset
  it "getOneCharPerRule" $
    getOneCharPerRule
      (26 * 26)
      defaultConfig { cDigits = False, cSymbols = False }
    `shouldBe`
    ("aA", 1)
  it "insertStringPseudoRandomly" $
    insertStringPseudoRandomly "123456" (7 * 6 + 2) "uT"
      `shouldBe` "T12u3456"
  it "genPassword with default options" $
    genPassword dat1 defaultConfig
      `shouldBe` "WHLpUL)e00[iHR+w"
  it "genPassword without symbols" $
    genPassword dat1 defaultConfig { cSymbols = False }
      `shouldBe` "Jyd57m7Ctw2695ks"
  it "genPassword without symbols and length=24" $
    genPassword dat1 defaultConfig { cSymbols = False, cLength = 24 }
      `shouldBe` "y5m7Ctw2696F5ksih1ZFmz3b"
  it "genPassword with length=12 and counter=14" $
    genPassword dat1 defaultConfig { cLength = 12, cCounter = 14 }
      `shouldBe` "k*EnKS\\9_wJm"
  it "genPassword with empty 'site'" $
    genPassword dat2 defaultConfig
      `shouldBe` "F)vWZa{GjMk`=5=T"
