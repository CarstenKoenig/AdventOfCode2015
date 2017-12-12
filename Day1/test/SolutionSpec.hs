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
    it "(()) is floor 0" $
      part1 "(())" `shouldBe` 0
    it "()() is floor 0" $
      part1 "()()" `shouldBe` 0
    it "(()(()( is floor 3" $
      part1 "(()(()(" `shouldBe` 3
    it ")())()) is floor -3" $
      part1 ")())())" `shouldBe` -3
  describe "part2" $ do
    it ") enters the cellar at 1" $
      part2 ")" `shouldBe` 1
    it "()()) should enter the cellar at 5" $
      part2 "()())" `shouldBe` 5
