module SolutionSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Solution

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "part1" $ do
    it " \"\" has code-length 2 but content-length 0" $ do
      let s = "\"\""
      codeLength s `shouldBe` 2
      contentLength s `shouldBe` 0
    it " \"aaa\\\"aaa\" has code-length 10 but content-length 7" $ do
      let s = "\"aaa\\\"aaa\""
      codeLength s `shouldBe` 10
      contentLength s `shouldBe` 7
    it " \"\\x27\" has code-length 6 but content-length 1" $ do
      let s = "\"\\x27\""
      codeLength s `shouldBe` 6
      contentLength s `shouldBe` 1
    it "part1 on example should be 12" $ do
      part1 (lines exampleInp) `shouldBe` 12


exampleInp :: String
exampleInp = "\"\"\n\"abc\"\n\"aaa\\\"aaa\"\n\"\\x27\""
