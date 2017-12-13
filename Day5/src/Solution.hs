module Solution (isNice, readInput, part1, part2)  where


type Input = [String]


part1 :: Input -> Int
part1 = length . filter isNice


part2 :: Input -> ()
part2 inp = ()


isNice :: String -> Bool
isNice s =
  contains3Vowels s && containsDouble s && not (containsNaughty s)


contains3Vowels :: String -> Bool
contains3Vowels inp =
  let vowelCount = length $ filter (`elem` "aeiou") inp
  in vowelCount >= 3


containsDouble :: Eq a => [a] -> Bool
containsDouble [] = False
containsDouble [_] = False
containsDouble (x:rest@(y:_))
  | x == y    = True
  | otherwise = containsDouble rest


containsNaughty :: String -> Bool
containsNaughty [] = False
containsNaughty [_] = False
containsNaughty (x:rest@(y:_))
  | [x,y] `elem` naughties = True
  | otherwise              = containsNaughty rest
  where
    naughties = ["ab", "cd", "pq", "xy"]


readInput :: IO Input
readInput = lines <$> readFile "input.txt"
