module Solution (readInput, part1, part2)  where

import Data.List (group)

type Input = Int

puzzleInput :: Input
puzzleInput = 33100000


part1 :: Input -> ()
part1 inp = ()


part2 :: Input -> ()
part2 inp = ()


readInput :: IO Input
readInput = return puzzleInput


primeFactors :: Integral a => a -> [(a, Int)]
primeFactors n = map factor . group $ collect n primes
  where
    factor ks@(k:_) = (k, length ks)
    collect k ps@(p:ps')
      | p > k = []
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
