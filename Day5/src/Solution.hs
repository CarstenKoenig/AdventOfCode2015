module Solution (isNice, readInput, part1, part2)  where


type Input = [String]


part1 :: Input -> Int
part1 = length . filter isNice


part2 :: Input -> Int
part2 = length . filter isNice2


isNice :: String -> Bool
isNice s =
  contains3Vowels s && containsDouble s && not (containsNaughty s)


isNice2 :: String -> Bool
isNice2 s = containsTripple s && containsTwoNonOverlapping s


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


containsTwoNonOverlapping :: String -> Bool
containsTwoNonOverlapping [] = False
containsTwoNonOverlapping [_] = False
containsTwoNonOverlapping (x:y:rest) =
   isSubstring [x,y] rest || containsTwoNonOverlapping (y:rest)


containsTripple :: String -> Bool
containsTripple [] = False
containsTripple [_] = False
containsTripple [_,_] = False
containsTripple (x:rest@(_:z:_)) = x == z || containsTripple rest


isSubstring :: String -> String -> Bool
isSubstring [] _ = True
isSubstring _ [] = False
isSubstring sub s =
  sub == take (length sub) s || isSubstring sub (drop 1 s)


readInput :: IO Input
readInput = lines <$> readFile "input.txt"
