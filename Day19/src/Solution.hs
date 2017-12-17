module Solution (readInput, part1, part2)  where

import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Parser

type Input = [T.Text]


data Problem =
  Problem { rules  :: Rules
          , target :: T.Text
          } deriving Show


type Rules = M.Map T.Text T.Text


part1 :: Input -> ()
part1 inp = ()


part2 :: Input -> ()
part2 inp = ()


readInput :: IO Input
readInput = T.lines <$> TIO.readFile "input.txt"


parseInput :: Input -> Problem
parseInput inp =
  let (ruleLines, (_:problemLine:_)) = splitAt (length inp - 2) inp
      rules = map (fromJust . eval ruleP . T.unpack) ruleLines
  in Problem (M.fromList rules) problemLine


ruleP :: Parser (T.Text, T.Text)
ruleP = (,) <$> (T.pack <$> parseAlphas <* ignoreWhiteSpace <* parseString "=>" <* ignoreWhiteSpace) <*> (T.pack <$> parseAlphas)
