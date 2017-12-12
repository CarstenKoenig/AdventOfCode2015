module Solution (readInput, part1, part2)  where


type Input = String


part1 :: Input -> Int
part1 = sum . map upDown


part2 :: Input -> ()
part2 inp = ()


upDown :: Char -> Int
upDown '(' = 1
upDown ')' = -1
upDown c   = error $"unexpected character: '" ++ [c] ++ "'"


readInput :: IO Input
readInput = readFile "input.txt"
