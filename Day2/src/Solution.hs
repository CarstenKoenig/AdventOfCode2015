{-# LANGUAGE OverloadedStrings #-}
module Solution (parseInput, readInput, part1, part2)  where

import Data.List (sort)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Input = [Dimension]

type Dimension = (Int, Int, Int)


part1 :: Input -> Int
part1 = sum . map wrapSquareFeed


part2 :: Input -> Int
part2 = sum . map ribbonLength


wrapSquareFeed :: Dimension -> Int
wrapSquareFeed (x,y,z) =
  let sides = [x*y, x*z, y*z]
      smallest = minimum sides
  in 2 * sum sides + smallest


ribbonLength :: Dimension -> Int
ribbonLength (x,y,z) =
  let sideLen = (2*) . sum . take 2 $ sort [x,y,z]
      volume  = x*y*z
  in sideLen + volume


readInput :: IO Input
readInput = parseInput <$> TIO.readFile "input.txt"


parseInput :: T.Text -> Input
parseInput = map (toDim . map T.unpack . T.splitOn "x") . T.lines
  where
    toDim [x,y,z] = (read x, read y, read z)
