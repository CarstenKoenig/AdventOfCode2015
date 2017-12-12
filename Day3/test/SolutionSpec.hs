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
    it "> should return 1" $
      part1 (parseInput ">") `shouldBe` 2
    it "^>v< should return 4" $
      part1 (parseInput "^>v<") `shouldBe` 4
    it "^v^v^v^v^v should return 2" $
      part1 (parseInput "^v^v^v^v^v") `shouldBe` 2

  describe "part2" $ do
    it "^v should return 3" $
      part2 (parseInput "^v") `shouldBe` 3
    it "^>v< should return 4" $
      part2 (parseInput "^>v<") `shouldBe` 3
    it "^v^v^v^v^v should return 11" $
      part2 (parseInput "^v^v^v^v^v") `shouldBe` 11
