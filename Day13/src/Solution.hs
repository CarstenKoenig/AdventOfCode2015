module Solution (readInput, part1, part2)  where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList, mapMaybe)
import Parser


type Input = [Connection]

type Graph = Map Person (Map Person Happiness)

type Connection = (Person, Person, Happiness)

type Table = [Sit]

data Sit
  = Sit { person  :: Person
        , happy   :: Happiness
        } deriving Show

type Person = String
type Happiness = Int


part1 :: Input -> Happiness
part1 = maximum . map totalHappiness . tables . parseInput


part2 :: Input -> ()
part2 = undefined


tables :: Graph -> [Table]
tables gr = genTables gr (persons gr)


totalHappiness :: Table -> Happiness
totalHappiness = sum . map happy


genTables :: Graph -> [Person] -> [Table]
genTables gr (p:ps) = do
  ps' <- lists ps
  let lst = last ps'
  foldSit (lst:p:ps' ++ [p])
  where
    foldSit :: [Person] -> [[Sit]]
    foldSit (prev:cur:next:rem) = do
      hl <- maybeToList $ happiness gr cur prev
      hr <- maybeToList $ happiness gr cur next
      rest <- foldSit (cur:next:rem)
      return $ (Sit cur (hl+hr)) : rest
    foldSit _ = [[]]


lists :: [a] -> [[a]]
lists [] = [[]]
lists xs = do
  (y,xs') <- pick xs
  ys      <- lists xs'
  return (y:ys)


persons :: Graph -> [Person]
persons = Map.keys


happiness :: Graph -> Person -> Person -> Maybe Happiness
happiness gr from to =
  Map.lookup from gr
  >>= Map.lookup to


addConnection :: Connection -> Graph -> Graph
addConnection (start, end, dist) = add start end
  where
    add a b = Map.insertWith Map.union a (Map.singleton b dist)


pick :: [a] -> [(a,[a])]
pick [] = []
pick (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- pick xs ]


readInput :: IO Input
readInput = mapMaybe (eval connectionP) . lines <$> readFile "input.txt"


readExample :: IO Input
readExample = mapMaybe (eval connectionP) . lines <$> readFile "example.txt"


parseInput :: Input -> Graph
parseInput = foldr addConnection Map.empty


connectionP :: Parser Connection
connectionP = do
  from <- personP
  parseString "would "
  hap <- happinessP
  parseString "happiness units by sitting next to "
  to <- personP
  parseString "."
  return (from,to,hap)


personP :: Parser Person
personP = parseAlphas <* ignoreWhiteSpace


happinessP :: Parser Happiness
happinessP = opP <*> parseInt
  where opP = parseEither (parseString "gain " *> pure id) (parseString "lose " *> pure negate)
