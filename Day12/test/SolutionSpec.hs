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
    it "[1,2,3] have sum 6" $ do
      part1 "[1,2,3]" `shouldBe` 6
    it "{'a':2,'b':4} have sum 6" $ do
      part1 "{\"a\":2,\"b\":4}" `shouldBe` 6
    it "[[[3]]] have sum 3" $ do
      part1 "[[[3]]]" `shouldBe` 3
    it "{'a':{'b':4},'c':-1} have sum 3" $ do
      part1 "{\"a\":{\"b\":4},\"c\":-1}" `shouldBe` 3
    it "{'a':[-1,1]} have sum 0" $ do
      part1 "{\"a\":[-1,1]}" `shouldBe` 0
    it "[-1,{'a':1}] have sum 0" $ do
      part1 "[-1,{\"a\":1}]" `shouldBe` 0
    it "[] have sum 0" $ do
      part1 "[]" `shouldBe` 0
    it "{} have sum 0" $ do
      part1 "{}" `shouldBe` 0


  describe "part2" $ do
    it "[1,2,3] have sum 6" $ do
      part2 "[1,2,3]" `shouldBe` 6
    it "[1,{'c':'red','b':2},3] have sum 4" $ do
      part2 "[1,{\"c\":\"red\",\"b\":2},3]" `shouldBe` 4
    it "{'d':'red','e':[1,2,3,4],'f':5} have sum 0" $ do
      part2 "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}" `shouldBe` 0
    it "[1,\"red\",5] have sum 6" $ do
      part2 "[1,\"red\",5]" `shouldBe` 6
