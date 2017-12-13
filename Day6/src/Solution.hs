module Solution (readInput, part1, part2)  where

import Prelude hiding (init)

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)

import Parser


----------------------------------------------------------------------
-- data and types

type Input = Program

type Program = [Command]

data Command =
  Command { operation   :: Operation 
          , topLeft     :: Coord
          , bottomRight :: Coord
          }

type Grid = Map Coord Int

type Coord = (Int, Int)

type Operation = Coord -> Grid -> Grid


----------------------------------------------------------------------
-- solutions

part1 :: Input -> String
part1 _ = "please see commit history"


part2 :: Input -> Int
part2 = totalBrightness . run


----------------------------------------------------------------------
-- interpreter

run :: Program -> Grid
run = foldl' (flip execute) init


execute :: Command -> Grid -> Grid
execute cmd = operate (operation cmd) (topLeft cmd, bottomRight cmd)


operate :: Operation -> (Coord, Coord) -> Grid -> Grid
operate op ((l,t),(r,b)) gr = foldl' (flip op) gr coords
  where coords = [ (x,y) | x <- [l..r], y <- [t..b] ]


----------------------------------------------------------------------
-- grid operations

init :: Grid
init = Map.empty


numbersLit :: Grid -> Int
numbersLit = length . filter (> 0) . Map.elems


totalBrightness :: Grid -> Int
totalBrightness = sum . Map.elems


switch :: (Int -> Int) -> Operation
switch f = Map.alter (Just . f . fromMaybe 0)


turnOn :: Operation
turnOn = switch (+ 1)


turnOff :: Operation
turnOff = switch (max 0 . (subtract 1))


toggle :: Operation
toggle = switch (+ 2)


----------------------------------------------------------------------
-- input and parsing

readInput :: IO Input
readInput = mapMaybe (eval commandP) . lines <$> readFile "input.txt"


commandP :: Parser Command
commandP =
  Command <$> operationP <*> coordP <*> (parseString "through " *> coordP)


operationP :: Parser Operation
operationP = parseOneOf [turnOnP, turnOffP, toggleP]
  where turnOnP = parseString "turn on " *> pure turnOn
        turnOffP = parseString "turn off " *> pure turnOff
        toggleP = parseString "toggle " *> pure toggle


coordP :: Parser Coord
coordP = (,) <$> (parseInt <* parseString ",") <*> parseInt
