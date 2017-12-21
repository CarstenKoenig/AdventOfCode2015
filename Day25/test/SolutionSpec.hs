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
  describe "coordToDiag" $ do
    it "for (2,3) is 9" $ do
      coordToDiag (2,3) `shouldBe` 9
    it "should return 21 for" $ do
      coordToDiag (1,6) `shouldBe` 21
    it "should return 16 for (6,1)" $ do
      coordToDiag (6,1) `shouldBe` 16
    it "should return 18 for (4,3)" $ do
      coordToDiag (4,3) `shouldBe` 18

  describe "step" $ do
    it "is 31916031 after start" $ do
      step start `shouldBe` 31916031

  describe "code" $ do
    it "is 31916031 after for (2,1)" $ do
      code (2,1) `shouldBe` 31916031
    it "is 31663883 after for (5,6)" $ do
      code (5,6) `shouldBe` 31663883

