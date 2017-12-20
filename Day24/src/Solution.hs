module Solution (readInput, part1, part2)  where

import Data.Function (on)
import Data.List (groupBy, sortBy)


type Input = [Package]

type Package = Int

part1 :: Input -> Int
part1 inp =
  let tw = targetWeight inp
      (best,_,_) = compartmentConfig $ pickGroups tw inp
  in quantumEntanglement best


part2 :: Input -> ()
part2 inp = ()


targetWeight :: Input -> Int
targetWeight inp = let total = sum inp in total `div` 3


quantumEntanglement :: [Int] -> Int
quantumEntanglement = product


compartmentConfig :: [([Int],[Int],[Int])] -> ([Int], [Int], [Int])
compartmentConfig =
  head . sortBy (compare `on` fstQE) .
  head . groupBy ((==) `on` fstLen). sortBy (compare `on` fstLen)
  where fstLen (xs,_,_) = length xs
        fstQE (xs,_,_) = quantumEntanglement xs


pickGroups :: Int -> [Int] -> [([Int],[Int],[Int])]
pickGroups tgt ws = go tgt ws
  where
    go t xs = do
      (grp1,xs')  <- pickGroup t xs
      (grp2,grp3) <- pickGroup t xs'
      return (grp1,grp2,grp3)


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
