module Solution (readInput, part1, part2)  where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList, mapMaybe)
import Parser


type Input = [Connection]

type Graph = Map City (Map City Distance)

type Connection = (City, City, Distance)

type City = String
type Distance = Int


part1 :: Input -> Distance
part1 = minimum . map fst . tours . parseInput


part2 :: Input -> ()
part2 inp = ()


tours :: Graph -> [(Distance, [City])]
tours gr = do
  (from, cities') <- pick (cities gr)
  go from cities'
  where
    go from [] = [(0, [from])]
    go from cities = do
      (to, cities') <- pick cities
      dist <- maybeToList $ distance gr from to
      (restDist, restTour) <- go to cities'
      return (dist+restDist, from:restTour)


cities :: Graph -> [City]
cities = Map.keys


distance :: Graph -> City -> City -> Maybe Distance
distance gr from to =
  Map.lookup from gr
  >>= Map.lookup to


addConnection :: Connection -> Graph -> Graph
addConnection (start, end, dist) = add start end . add end start
  where
    add a b = Map.insertWith Map.union a (Map.singleton b dist)


pick :: [a] -> [(a,[a])]
pick [] = []
pick (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- pick xs ]


readInput :: IO Input
readInput = mapMaybe (eval connectionP) . lines <$> readFile "input.txt"


parseInput :: Input -> Graph
parseInput = foldr addConnection Map.empty


connectionP :: Parser Connection
connectionP = do
  from <- cityP
  parseString "to "
  to <- cityP
  parseString "= "
  dist <- parseInt
  return (from,to,dist)

cityP :: Parser City
cityP = parseAlphas  <* ignoreWhiteSpace
