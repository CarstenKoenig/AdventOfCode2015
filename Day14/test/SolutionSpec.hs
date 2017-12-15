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
  describe "race" $ do
    it "should return 1120 for the test-input" $ do
      snd (head . race 1000 $ parseInput testInput) `shouldBe` 1120
  describe "points" $ do
    it "should return 689 for the test-input" $ do
      points 1000 (parseInput testInput) `shouldBe` 689


testInput :: String
testInput = unlines
  [ "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
  , "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
  ]
