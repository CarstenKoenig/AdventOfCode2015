module Solution (readInput, part1, part2)  where


import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)

import Parser

type Input = [Sue]

data Sue =
  Sue
  { number      :: Int
  , things      :: Things
  } deriving Show


type Things = Map String Int
type Matches = Map String Match
type Match = Int -> Bool


part1 :: Input -> Int
part1 = number . head . filter (isExactMatch . things)


part2 :: Input -> Int
part2 = number . head . filter (isMatch . things)


isExactMatch :: Things -> Bool
isExactMatch = all thingMatch . Map.toList
  where
    thingMatch (name, count) =
      Map.lookup name analysedThings == Just count


isMatch :: Things -> Bool
isMatch = all thingMatch . Map.toList
  where
    thingMatch (name, count) =
      fromMaybe (const True) (Map.lookup name analysedMatches) $ count


analysedThings :: Things
analysedThings = Map.fromList
  [ ("children", 3)
  , ("cats", 7)
  , ("samoyeds", 2)
  , ("pomeranians", 3)
  , ("akitas", 0)
  , ("vizslas", 0)
  , ("goldfish", 5)
  , ("trees", 3)
  , ("cars", 2)
  , ("perfumes", 1)
  ]


analysedMatches :: Matches
analysedMatches = Map.fromList
  [ ("children", (== 3))
  , ("cats", (> 7))
  , ("samoyeds", (== 2))
  , ("pomeranians", (< 3))
  , ("akitas", (== 0))
  , ("vizslas", (== 0))
  , ("goldfish", (< 5))
  , ("trees", (> 3))
  , ("cars", (== 2))
  , ("perfumes", (== 1))
  ]


readInput :: IO Input
readInput = map (fromJust . eval sueP) . lines <$> readFile "input.txt"


sueP :: Parser Sue
sueP = Sue <$> numberP <*> thingsP


numberP :: Parser Int
numberP = parseString "Sue " *> parseInt <* parseString ":" <* ignoreWhiteSpace


thingsP :: Parser Things
thingsP = Map.fromList <$> thingP `parseSepBy` (parseChar ',' <* ignoreWhiteSpace)


thingP :: Parser (String, Int)
thingP = (,) <$> (parseAlphas <* parseChar ':' <* ignoreWhiteSpace) <*> parseInt
