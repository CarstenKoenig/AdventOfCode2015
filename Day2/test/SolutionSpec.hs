{-# LANGUAGE OverloadedStrings #-}

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
    it "2x3x4 should return 58" $
      part1 (parseInput "2x3x4")  `shouldBe` 58
    it "2x3x4 and 1x1x10 should return 58+43" $
      part1 (parseInput "2x3x4\n1x1x10") `shouldBe` (58+43)

  describe "part2" $ do
    it "ribbon length for parcel 2x3x4 is 34 feet" $
      part2 (parseInput "2x3x4") `shouldBe` 34
    it "ribbon length for parcels 2x3x4 and 1x1x10 = 34+14 feet" $
      part2 (parseInput "2x3x4\n1x1x10") `shouldBe` 48
