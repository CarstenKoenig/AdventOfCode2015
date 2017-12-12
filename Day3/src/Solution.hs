module Solution (parseInput, readInput, part1, part2)  where

import Prelude hiding (Left, Right)

import Data.List (scanl')
import Data.Monoid (Sum(..), (<>))
import Data.Set (Set)
import qualified Data.Set as Set

type Input = [Direction]

type Path = [Coord]

data Direction = Up | Right | Down | Left

type Coord = (Sum Int, Sum Int)


part1 :: Input -> Int
part1 = Set.size . visited . path


part2 :: Input -> ()
part2 inp = ()


visited :: Path -> Set Coord
visited = Set.fromList


path :: Input -> Path
path = scanl' (<>) mempty . map move
  where
    move Up    = (Sum 0, Sum 1)
    move Down  = (Sum 0, Sum (-1))
    move Left  = (Sum (-1), Sum 0)
    move Right = (Sum 1, Sum 0)


readInput :: IO Input
readInput = parseInput <$> readFile "input.txt"


parseInput :: String -> Input
parseInput = map parseChar
  where
    parseChar '^' = Up
    parseChar '>' = Right
    parseChar 'v' = Down
    parseChar '<' = Left
