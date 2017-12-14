module Main where

import Solution

main :: IO ()
main = do
  inp <- readInput

  putStrLn $ "part 1: " ++ show (part1 inp)
  putStrLn $ "part 2: " ++ show (part2 inp)