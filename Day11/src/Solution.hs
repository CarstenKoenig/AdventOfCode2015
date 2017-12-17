module Solution (readInput, part1, part2)  where

import Data.Char (ord, chr)
import Data.Digits (digits, unDigits)


type Input = Password

newtype Password = PW String
  deriving Show


part1 :: Input -> Password
part1 = nextPassword


part2 :: Input -> Password
part2 inp = nextPassword $ nextPassword inp


nextPassword :: Password -> Password
nextPassword pw =
  let candiate = next pw
  in if verifyPassword candiate then candiate else nextPassword candiate


next :: Password -> Password
next (PW s) =
  let s' = fromNumber $ toNumber s + 1
      s'' = replicate (length s - length s') 'a' ++ s'
  in PW s''


toNumber :: String -> Integer
toNumber = unDigits 26 . map (\c -> fromIntegral (ord c - ord 'a'))


fromNumber :: Integer -> String
fromNumber 0 = "a"
fromNumber n = toChar <$> digits 26 n
  where toChar n = chr (ord 'a' + fromIntegral n)


verifyPassword :: Password -> Bool
verifyPassword (PW s) = verify s
  where verify ns = notIOL ns && straight ns && pairCount ns > 1


straight :: (Eq a, Enum a) => [a] -> Bool
straight []                    = False
straight [_]                   = False
straight [_,_]                 = False
straight (a:b:c:rest)
  | succ a == b && succ b == c = True
  | otherwise                  = straight (b:c:rest)


notIOL :: [Char] -> Bool
notIOL = not . any (`elem` "iol")


pairCount :: Eq a => [a] -> Int
pairCount []  = 0
pairCount [_] = 0
pairCount (x:y:rest)
  | x == y    = 1 + pairCount rest
  | otherwise = pairCount (y:rest)


readInput :: IO Input
readInput = return (PW "vzbxkghb")
