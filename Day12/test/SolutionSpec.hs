module SolutionSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Solution

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


eval :: String -> Int
eval = sumNumbers . parseInput

spec :: Spec
spec = do
  describe "part1" $ do
    it "[1,2,3] have sum 6" $ do
      eval "[1,2,3]" `shouldBe` 6
    it "{'a':2,'b':4} have sum 6" $ do
      eval "{\"a\":2,\"b\":4}" `shouldBe` 6
    it "[[[3]]] have sum 3" $ do
      eval "[[[3]]]" `shouldBe` 3
    it "{'a':{'b':4},'c':-1} have sum 3" $ do
      eval "{\"a\":{\"b\":4},\"c\":-1}" `shouldBe` 3
    it "{'a':[-1,1]} have sum 0" $ do
      eval "{\"a\":[-1,1]}" `shouldBe` 0
    it "[-1,{'a':1}] have sum 0" $ do
      eval "[-1,{\"a\":1}]" `shouldBe` 0
    it "[] have sum 0" $ do
      eval "[]" `shouldBe` 0
    it "{} have sum 0" $ do
      eval "{}" `shouldBe` 0
