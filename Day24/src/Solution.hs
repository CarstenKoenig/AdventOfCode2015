module Solution (readInput, part1, part2)  where

import Data.Function (on)
import Data.List (groupBy, sort)
import qualified Control.Monad.State.Strict as S

type Input = [Package]

type Package = Int


part1 :: Input -> Int
part1 = calculateResult 3


part2 :: Input -> Int
part2 = calculateResult 4


calculateResult :: Int -> Input -> Int
calculateResult n inp =
  let tw = targetWeight n inp
  in compartmentConfig $ pickGroups n tw (reverse inp)


targetWeight :: Int -> Input -> Int
targetWeight n inp = let total = sum inp in total `div` n


quantumEntanglement :: [Int] -> Int
quantumEntanglement = product


compartmentConfig :: [[Int]] -> Int
compartmentConfig =
  minimum . map quantumEntanglement .
  head . groupBy ((==) `on` length)


pickGroups :: Int -> Int -> [Int] -> [[Int]]
pickGroups n tgt ws = concatMap (\ l -> go l tgt ws) [1..]
  where
    go smallest t xs = do
      (grp1,xs')  <- pickGroupWithLen smallest t xs
      _ <- take 1 $ pickNGroups (n-1) t xs'
      return grp1


pickGroupWithLen :: Int -> Int -> [Int] -> [([Int], [Int])]
pickGroupWithLen = go
  where go 0 0 xs = return ([], xs)
        go _ _ [] = []
        go 0 _ (_:_) = []
        go l t (x:xs)
          | x > t = [ (y,x:ys) | (y,ys) <- go l t xs ]
          | otherwise =
              [ (x:ys, rem) | (ys,rem) <- go (l-1) (t-x) xs ]
              ++ [ (y,x:ys) | (y,ys) <- go l t xs ]


pickNGroups :: Int -> Int -> [Int] -> [[[Int]]]
pickNGroups 0 _ _ = [[]]
pickNGroups n tgt ws = do
  (grp,ws') <- pickGroup tgt ws
  grps <- pickNGroups (n-1) tgt ws'
  return (grp:grps)


pickGroup :: Int -> [Int] -> [([Int], [Int])]
pickGroup = go
  where go 0 xs = return ([], xs)
        go _ [] = []
        go t (x:xs)
          | x > t = [ (y,x:ys) | (y,ys) <- go t xs ]
          | otherwise =
              [ (x:ys, rem) | (ys,rem) <- go (t-x) xs ]
              ++ [ (y,x:ys) | (y,ys) <- go t xs ]


readInput :: IO Input
readInput = map read . lines <$> readFile "input.txt"
