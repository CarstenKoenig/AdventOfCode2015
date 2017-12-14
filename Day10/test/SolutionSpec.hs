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
  describe "step" $ do
    it "should return 11 for 1" $ do
      step "1" `shouldBe` "11"
    it "should return 21 for 11" $ do
      step "11" `shouldBe` "21"
    it "should return 1211 for 21" $ do
      step "21" `shouldBe` "1211"
    it "should return 111221 for 1211" $ do
      step "1211" `shouldBe` "111221"
    it "should return 312211 for 111221" $ do
      step "111221" `shouldBe` "312211"
