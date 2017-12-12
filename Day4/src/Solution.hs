module Solution (readInput, part1, part2)  where

import Crypto.Hash
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Data.Function (on)


type Input = String


secretKey :: Input
secretKey = "yzbqklnj"


part1 :: Input -> Int
part1 = findHash 5


part2 :: Input -> Int
part2 = findHash 6


findHash :: Int -> Input -> Int
findHash zeroes key =
  fst . head
  $ filter (startsWith prefix . snd)
  $ map (fmap md5Hash)
  $ attempts key
  where prefix = replicate zeroes '0'


attempts :: Input -> [(Int, String)]
attempts key = [ (i, key ++ show i) | i <- [0..] ]


startsWith :: String -> String -> Bool
startsWith prefix s = take (length prefix) s == prefix


readInput :: IO Input
readInput = return secretKey


md5Hash :: String -> String
md5Hash s = show (hash (pack s) :: Digest MD5)
