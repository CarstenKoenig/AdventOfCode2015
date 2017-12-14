module Solution (step, readInput, part1, part2)  where

import Data.List (group)

type Input = String


part1 :: Input -> Int
part1 = length . run 40


part2 :: Input -> Int
part2 = length . run 50


run :: Int -> Input -> Input
run n = nTimes n step

step :: Input -> Input
step = concatMap writeGroup . group
  where
    writeGroup (gr@(x:_)) = show (length gr) ++ [x]


nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ x = x
nTimes n f x = nTimes (n-1) f (f x)


readInput :: IO Input
readInput = return "1113122113"
