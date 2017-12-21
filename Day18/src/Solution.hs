module Solution (readInput, part1, part2)  where

import qualified Data.IntSet as IS

type Input = Grid

type Coord = (Int,Int)

type Size = (Int,Int)

type Grid = IS.IntSet


part1 :: Input -> Int
part1 inp = IS.size $ run inp 100


part2 :: Input -> ()
part2 inp = ()


run :: Input -> Int -> Grid
run grd n = nTimes n step grd


size :: Size
size = (100,100)


coords :: [Coord]
coords = [ (x,y) | y <- [0..99], x <- [0..99] ]


step :: Grid -> Grid
step grd = IS.fromList . map toGridIndex $ [ c | c <- coords, isOnNext grd c ]


isOnNext :: Grid -> Coord -> Bool
isOnNext grd coord
  | isOn grd coord = litNeighs == 2 || litNeighs == 3
  | otherwise      = litNeighs == 3
  where litNeighs = litNeighbours grd coord


litNeighbours :: Grid -> Coord -> Int
litNeighbours grd coord = length . filter (isOn grd) $ neighbours coord


neighbours :: Coord -> [Coord]
neighbours (x,y) =
  [ (x-1,y-1), (x,y-1), (x+1,y-1)
  , (x-1,y)  ,          (x+1,y)
  , (x-1,y+1), (x,y+1), (x+1,y+1)
  ]


isOn :: Grid -> Coord -> Bool
isOn grd coord@(x,y)
  | x < 0 || x >= 100 || y < 0 || y >= 100 = False
  | otherwise = toGridIndex coord `IS.member` grd


toGridIndex :: Coord -> Int
toGridIndex (x,y) = y * 100 + x


nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ x = x
nTimes n f x = nTimes (n-1) f (f x)


readInput :: IO Input
readInput = parse . lines <$> readFile "input.txt"


parse :: [String] -> Grid
parse ls =
  let lines = zip [0..] ls
      coords = concatMap (\ (y,cs) -> zipWith (select y) [0..] cs) lines
  in IS.fromList $ map (toGridIndex . fst) $ filter snd $ coords
  where
    select y x c = ((x,y), c == '#')
