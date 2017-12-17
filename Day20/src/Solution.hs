module Solution (readInput, part1, part2)  where

import Data.List (group)

type Input = Int

puzzleInput :: Input
puzzleInput = 33100000


part1 :: Input -> Int
part1 inp = head $ filter (\ nr -> housePresents nr >= inp) [1..]


part2 :: Input -> Int
part2 inp = head $ filter (\ nr -> housePresents2 nr >= inp) [1..]


housePresents :: Int -> Int
housePresents nr = 10 * sigma nr


housePresents2 :: Int -> Int
housePresents2 n = 11 * sum (relevantFactors n)


relevantFactors :: Int -> [Int]
relevantFactors n = filter relevant $ factors n
  where relevant d = n `div` d <= 50


readInput :: IO Input
readInput = return puzzleInput


type PrimeFactors a = [(a, Int)]


sigma :: Integral a => a -> a
sigma = sigma' . primeFactors


sigma' :: Num a => PrimeFactors a -> a
sigma' = product . map groupSum
  where groupSum (p, r) = sum . take (r+1) $ iterate (* p) 1


factors :: Integral a => a -> [a]
factors = factors' . primeFactors


factors' :: Num a => PrimeFactors a -> [a]
factors' pf = go pf
  where go [] = [1]
        go ((p,k):ps) = do
          i <- [0..k]
          d <- go ps
          return $ p^i * d

primeFactors :: Integral a => a -> PrimeFactors a
primeFactors n = map factor . group $ collect n primes
  where
    factor ks@(k:_) = (k, length ks)
    collect k ps@(p:ps')
      | p > k     = []
      | isPrime k = [k]
      | otherwise =
        let (d,m) = k `divMod` p
        in if m == 0
           then p : collect d ps
           else collect k ps'


primes :: Integral a => [a]
primes = 2 : filter isPrime [3..]


isPrime :: Integral a => a -> Bool
isPrime n = not . any (`divides` n) $ takeWhile (\i -> i*i <= n) primes


divides :: Integral a => a -> a -> Bool
divides d n = n `mod` d == 0
