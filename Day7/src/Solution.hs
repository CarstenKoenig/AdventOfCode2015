module Solution (signal, run, parseInput, readInput, part1, part2)  where


type Input = String

type Program = ()

type Result = ()

type SignalId = String


part1 :: Input -> ()
part1 inp = ()


part2 :: Input -> ()
part2 inp = ()


run :: Program -> Result
run = undefined


signal :: Result -> SignalId -> Int
signal = undefined


readInput :: IO Input
readInput = readFile "input.txt"


parseInput :: Input -> Program
parseInput = undefined
