module Solution (race, points, parseInput, readInput, part1, part2)  where

import Control.Monad (forM_)
import Control.Monad.State (State)
import qualified Control.Monad.State as State
import Data.Function (on)
import Data.List (sortBy, groupBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Parser

type Input = String

type Points = Map Name Point

data Reindeer =
  Reindeer { name       :: Name
           , speedKmS   :: Speed
           , enduranceS :: Timespan
           , restS      :: Timespan
           } deriving Show

type Race a = State Points a

type Name = String
type Speed = Int
type Timespan = Int
type Distance = Int
type Point = Int


part1 :: Input -> Distance
part1 = snd . head . race 2503 . parseInput


part2 :: Input -> Point
part2 = points 2503 . parseInput


points :: Timespan -> [Reindeer] -> Point
points span = maximum . Map.elems . (flip State.execState Map.empty) . pointRace span


race :: Timespan -> [Reindeer] -> [(Reindeer, Distance)]
race span =
  head . groupBy ((==) `on` snd) . sortBy (flip compare `on` snd) . map (cruiseRange span)


pointRace :: Timespan -> [Reindeer] -> Race ()
pointRace span rs =
  forM_ [1..span] (\t -> let winners = fst <$> race t rs in addPoints winners)


addPoints :: [Reindeer] -> Race ()
addPoints rs = forM_ rs addPoint


addPoint :: Reindeer -> Race ()
addPoint r = State.modify (Map.insertWith (+) (name r) 1)


cruiseRange :: Timespan -> Reindeer -> (Reindeer, Distance)
cruiseRange span reind = (reind, totalCruiseSpan * speedKmS reind)
  where totalCruiseSpan = completeRuns * enduranceS reind + lastRunSpan
        (completeRuns, remSpan) = span `divMod` (enduranceS reind + restS reind)
        lastRunSpan = min remSpan (enduranceS reind)


readInput :: IO Input
readInput = readFile "input.txt"


parseInput :: Input -> [Reindeer]
parseInput = map (fromJust . eval reindeerP) . lines


reindeerP :: Parser Reindeer
reindeerP = do
  n <- parseAlphas <* ignoreWhiteSpace
  parseString "can fly "
  sp <- parseInt
  parseString "km/s for "
  en <- parseInt
  parseString "seconds, but then must rest for "
  p <- parseInt
  parseString "seconds."
  return $ Reindeer n sp en p
