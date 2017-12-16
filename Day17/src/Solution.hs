module Solution (readInput, part1, part2)  where

import Data.Function (on)
import Data.List (sortBy, groupBy)

type Input = [Container]


type Container = Int


part1 :: Input -> Int
part1 = length . combinations 150


part2 :: Input -> Int
part2 = length . head . groupByCount . combinations 150


groupByCount :: [[Container]] -> [[[Container]]]
groupByCount = groupBy ((==) `on` length) . sortBy (compare `on` length)


combinations :: Int -> [Container] -> [[Container]]
combinations 0 _  = [[]]
combinations _ [] = []
combinations vol (c:cs) =
  let without = combinations vol cs
      with    = (c:) <$> combinations (vol - c) cs
  in if c <= vol then with ++ without else without


readInput :: IO Input
readInput = map read . lines <$> readFile "input.txt"


example :: Input
example = [20, 15, 10, 5, 5]
