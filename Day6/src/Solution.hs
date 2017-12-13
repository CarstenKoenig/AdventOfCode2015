module Solution (readInput, part1, part2)  where

import Prelude hiding (init)

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)

import Parser


type Input = Program

type Program = [Command]

data Command =
  Command { operation   :: Operation 
          , topLeft     :: Coord
          , bottomRight :: Coord
          }

type Grid = Map Coord Bool

type Coord = (Int, Int)

type Operation = Coord -> Grid -> Grid


part1 :: Input -> Int
part1 = numbersLit . run


part2 :: Input -> ()
part2 inp = ()


run :: Program -> Grid
run = foldl' (flip execute) init


execute :: Command -> Grid -> Grid
execute cmd = operate (operation cmd) (topLeft cmd, bottomRight cmd)


operate :: Operation -> (Coord, Coord) -> Grid -> Grid
operate op ((l,t),(r,b)) gr = foldl' (flip op) gr coords
  where coords = [ (x,y) | x <- [l..r], y <- [t..b] ]


init :: Grid
init = Map.empty


numbersLit :: Grid -> Int
numbersLit = length . filter id . Map.elems


switch :: (Bool -> Bool) -> Operation
switch f = Map.alter (Just . f . fromMaybe False)


turnOn :: Operation
turnOn = switch (const True)


turnOff :: Operation
turnOff = switch (const False)


toggle :: Operation
toggle = switch not


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
