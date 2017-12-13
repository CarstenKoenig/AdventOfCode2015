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
  describe "isNice" $ do
    it "ugknbfddgicrmopn is nice" $ do
      isNice "ugknbfddgicrmopn" `shouldBe` True
    it "aaa is nice" $ do
      isNice "aaa" `shouldBe` True
    it "jchzalrnumimnmhp is naughty" $ do
      isNice "jchzalrnumimnmhp" `shouldBe` False
    it "haegwjzuvuyypxyu is naughty" $ do
      isNice "haegwjzuvuyypxyu" `shouldBe` False
    it "dvszwmarrgswjxmb is naughty" $ do
      isNice "haegwjzuvuyypxyu" `shouldBe` False
