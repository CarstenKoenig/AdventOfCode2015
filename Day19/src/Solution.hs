{-# LANGUAGE OverloadedStrings,
    BangPatterns #-}

module Solution (readInput, part1, part2)  where

import Data.Char (isUpper, isLower)
import qualified Data.HashSet as H
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, maybeToList)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Graph.AStar

import Parser

type Input = [T.Text]


data Problem =
  Problem { rules  :: Rules
          , target :: T.Text
          } deriving Show


type Rules = [(T.Text, T.Text)]


part1 :: Input -> Int
part1 inp =
  let p = parseInput inp
  in H.size $ allReplacements (rules p) (target p)


part2 :: Input -> Int
part2 = cheat


astar :: Input -> Int
astar inp =
  let (Problem rs tgt) = parseInput inp
  in fromJust . fmap length $ aStar (allReplacements rs) (\_ _ -> 1) (heur tgt) (== tgt) "e"
  where
    heur tgt !t = T.length tgt - T.length t


-- | the assumed A* algorithm will soon run out of memory (and my time)
-- so I actually reverted back to the "cheat" (actual it's an insight on the)
-- problem/input but well
-- see https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/
cheat :: Input -> Int
cheat inp =
  let p = parseInput inp
      els = elements (target p)
      total = length els
      rns   = length $ filter (== "Rn") els
      ars   = length $ filter (== "Ar") els
      ys    = length $ filter (== "Y") els
  in total - rns - ars - 2*ys - 1

allReplacements :: Rules -> T.Text -> H.HashSet T.Text
allReplacements rs tgt = H.fromList go
  where go = do
          pos <- [0..T.length tgt]
          let (pre, rest) = T.splitAt pos tgt
          (from, to) <- rs
          let (vgl, rest') = T.splitAt (T.length from) rest
          if from == vgl then return $ T.concat [ pre, to, rest' ] else []


readInput :: IO Input
readInput = T.lines <$> TIO.readFile "input.txt"


elements :: T.Text -> [T.Text]
elements = fromJust . eval (parseMany elementP) . T.unpack


parseInput :: Input -> Problem
parseInput inp =
  let (ruleLines, (_:problemLine:_)) = splitAt (length inp - 2) inp
      rules = map (fromJust . eval ruleP . T.unpack) ruleLines
  in Problem rules problemLine


ruleP :: Parser (T.Text, T.Text)
ruleP = (,) <$> (T.pack <$> parseAlphas <* ignoreWhiteSpace <* parseString "=>" <* ignoreWhiteSpace) <*> (T.pack <$> parseAlphas)


elementP :: Parser T.Text
elementP = do
  c <- parsePred isUpper
  rest <- parseMany (parsePred isLower)
  return $ T.pack (c:rest)
