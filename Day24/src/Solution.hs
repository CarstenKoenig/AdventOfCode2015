module Solution (readInput, part1, part2)  where

import Control.Monad (guard)
import Data.Function (on)
import Data.List (groupBy, sortBy)
import qualified Control.Monad.State.Strict as S
import Debug.Trace

type Input = [Package]

type Package = Int

part1 :: Input -> Int
part1 inp =
  let tw = targetWeight inp
      (best,_,_) = compartmentConfig $ pickGroups tw (reverse inp)
  in quantumEntanglement best


part2 :: Input -> Int
part2 inp =
  let tw = targetWeight2 inp
      (best,_,_,_) = compartmentConfig2 $ pickGroups2 tw (reverse inp)
  in quantumEntanglement best


targetWeight :: Input -> Int
targetWeight inp = let total = sum inp in total `div` 3


targetWeight2 :: Input -> Int
targetWeight2 inp = let total = sum inp in total `div` 4


quantumEntanglement :: [Int] -> Int
quantumEntanglement = product


compartmentConfig :: [([Int],[Int],[Int])] -> ([Int], [Int], [Int])
compartmentConfig =
  head . sortBy (compare `on` fstQE) .
  head . groupBy ((==) `on` fstLen)
  where fstLen (xs,_,_) = length xs
        fstQE (xs,_,_) = quantumEntanglement xs


compartmentConfig2 :: [([Int],[Int],[Int],[Int])] -> ([Int], [Int], [Int], [Int])
compartmentConfig2 =
  head . sortBy (compare `on` fstQE) .
  head . groupBy ((==) `on` fstLen)
  where fstLen (xs,_,_,_) = length xs
        fstQE (xs,_,_,_) = quantumEntanglement xs


pickGroups2 :: Int -> [Int] -> [([Int],[Int],[Int],[Int])]
pickGroups2 tgt ws = concatMap (\ l -> go l tgt ws) [1..]
  where
    go smallest t xs = do
      (grp1,xs')  <- pickGroupWithLen smallest t xs
      (grp2,grp3,grp4) <- take 1 $ rest t xs'
      return (grp1,grp2,grp3,grp4)
    rest t xs' = do
      (grp2,xs'') <- pickGroup t xs'
      (grp3,grp4) <- pickGroup t xs'
      return (grp2,grp3,grp4)


pickGroups :: Int -> [Int] -> [([Int],[Int],[Int])]
pickGroups tgt ws = concatMap (\ l -> go l tgt ws) [1..]
  where
    go smallest t xs = do
      (grp1,xs')  <- pickGroupWithLen smallest t xs
      (grp2,grp3) <- take 1 $ rest t xs'
      return (grp1,grp2,grp3)
    rest t xs' = do 
      (grp2,grp3) <- pickGroup t xs'
      return (grp2,grp3)


pickGroupWithLen :: Int -> Int -> [Int] -> [([Int], [Int])]
pickGroupWithLen len tgt ws = go len tgt ws
  where go 0 0 xs = return $ ([], xs)
        go _ _ [] = []
        go 0 _ (_:_) = []
        go l t (x:xs)
          | x > t = [ (y,x:ys) | (y,ys) <- go l t xs ]
          | otherwise =
              [ (x:ys, rem) | (ys,rem) <- go (l-1) (t-x) xs ]
              ++ [ (y,x:ys) | (y,ys) <- go l t xs ]


pickGroup :: Int -> [Int] -> [([Int], [Int])]
pickGroup tgt ws = go tgt ws
  where go 0 xs = return $ ([], xs)
        go _ [] = []
        go t (x:xs)
          | x > t = [ (y,x:ys) | (y,ys) <- go t xs ]
          | otherwise =
              [ (x:ys, rem) | (ys,rem) <- go (t-x) xs ]
              ++ [ (y,x:ys) | (y,ys) <- go t xs ]


pick :: [a] -> [(a,[a])]
pick [] = []
pick (a:as) = (a,as) : [ (y,a:ys) | (y,ys) <- pick as ]


readInput :: IO Input
readInput = map read . lines <$> readFile "input.txt"
