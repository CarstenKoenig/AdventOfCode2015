module Solution (sumNumbers, parseInput, readInput, part1, part2)  where

import Data.Maybe (fromJust)

import Parser


type Input = String

data Json
  = JNumber Int
  | JString String
  | JArray [Json]
  | JObject [(String, Json)]
  deriving Show


part1 :: Input -> Int
part1 = sumNumbers . parseInput


part2 :: Input -> ()
part2 inp = ()


sumNumbers :: Json     -> Int
sumNumbers (JNumber n)  = n
sumNumbers (JString _)  = 0
sumNumbers (JArray xs)  = sum $ map sumNumbers xs
sumNumbers (JObject xs) = sum $ map (sumNumbers . snd) xs


parseInput :: Input -> Json
parseInput = fromJust . eval jsonP


readInput :: IO Input
readInput = readFile "input.txt"


jsonP :: Parser Json
jsonP = parseOneOf [ JNumber <$> parseInt
                   , JString <$> stringP
                   , arrayP
                   , objP
                   ]


objP :: Parser Json
objP = JObject <$> parseBetween start end (parseSepBy fieldP sep)
  where
    start = parsePred (== '{') <* ignoreWhiteSpace
    end   = parsePred (== '}') <* ignoreWhiteSpace
    sep   = parsePred (== ',') <* ignoreWhiteSpace


fieldP :: Parser (String, Json)
fieldP = (,) <$> stringP <* parsePred (== ':') <* ignoreWhiteSpace <*> jsonP


arrayP :: Parser Json
arrayP = JArray <$> parseBetween start end (parseSepBy jsonP sep)
  where
    start = parsePred (== '[') <* ignoreWhiteSpace
    end   = parsePred (== ']') <* ignoreWhiteSpace
    sep   = parsePred (== ',') <* ignoreWhiteSpace



stringP :: Parser String
stringP = parseBetween start start (parseMany $ parsePred (/= '\"'))
  where start = parsePred (== '\"') <* ignoreWhiteSpace

