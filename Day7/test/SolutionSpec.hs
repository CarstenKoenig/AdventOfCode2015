module SolutionSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Solution

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

{-
d: 72
e: 507
f: 492
g: 114
h: 65412
i: 65079
x: 123
y: 456
-}

spec :: Spec
spec = do
  describe "part1" $
    context "values after running the example" $ do
      let prg = parseInput exampleInp
          res = run prg
      it "d should be 72" $ do
        signal res "d" `shouldBe` 72
      it "e should be 507" $ do
        signal res "e" `shouldBe` 507
      it "f should be 492" $ do
        signal res "f" `shouldBe` 492
      it "h should be 65412" $ do
        signal res "h" `shouldBe` 65412
      it "a should be 123" $ do
        signal res "a" `shouldBe` 123
      it "b should be 1" $ do
        signal res "b" `shouldBe` 1


exampleInp :: String
exampleInp = unlines 
  [ "123 -> x"
  , "x -> a"
  , "456 -> y"
  , "x AND y -> d"
  , "x OR y -> e"
  , "x LSHIFT 2 -> f"
  , "y RSHIFT 2 -> g"
  , "NOT x -> h"
  , "NOT y -> i"
  , "1 AND a -> b"
  ]
