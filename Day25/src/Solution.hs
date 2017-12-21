{-# LANGUAGE BangPatterns #-}

module Solution (Coord, coordToDiag, code, step, start, readInput, part1, part2)  where

type Input = Coord


type Coord = (Int, Int)


code :: Input -> Int
code k =
  let n     = keyIter k
  in nTimes (n-1) step start


key :: Input
key = (3010, 3019)


keyIter :: Input -> Int
keyIter = coordToDiag


part1 :: Input -> Int
part1 inp = code inp


part2 :: Input -> ()
part2 inp = ()


start :: Int
start = 20151125


step :: Int -> Int
step n = n * 252533 `mod` 33554393


coordToDiag :: (Int, Int) -> Int
coordToDiag (row,col) = rowStart + addCol
  where rowStart = 1 + (row*(row-1) `div` 2)
        addCol   = ((col+row-1)*(col+row) `div` 2) - (row*(row+1) `div` 2)


nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ !x = x
nTimes n f !x = nTimes (n-1) f (f x)


readInput :: IO Input
readInput = return key
