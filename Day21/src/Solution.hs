module Solution (readInput, part1, part2)  where


type Input = Boss

puzzleInput :: Input
puzzleInput = Boss 1 8 104


data Boss =
  Boss
  { bossArmor  :: Int
  , bossDamage :: Int
  , bossHealth :: Int
  } deriving Show


data Equipment =
  Equipment
  { eqName   :: String
  , eqCost   :: Int
  , eqDamage :: Int
  , eqArmor  :: Int
  } deriving Show


data Player =
  Player
  { equipment :: [Equipment]
  , health    :: Int
  } deriving Show


part1 :: Input -> Int
part1 boss = minimum . map equipCost . filter (wins boss) $ map initPlayer outfits


part2 :: Input -> ()
part2 inp = ()


wins :: Boss -> Player -> Bool
wins (Boss ba bd bh) pl =
  let plRounds = ceiling (fromIntegral bh / fromIntegral plDealt)
      bossRounds = ceiling (fromIntegral (health pl) / fromIntegral bossDealt)
      plDealt = max 0  (playerDamage pl - ba)
      bossDealt = max 0 (bd - playerArmor pl)
  in plRounds <= bossRounds


outfits :: [[Equipment]]
outfits = do
  ws <- weaponPicks
  as <- armorPicks
  rs <- ringPicks
  pure $ ws ++ as ++ rs


initPlayer :: [Equipment] -> Player
initPlayer eq = Player eq 100


playerDamage :: Player -> Int
playerDamage = sum . map eqDamage . equipment


playerArmor :: Player -> Int
playerArmor = sum . map eqArmor . equipment


equipCost :: Player -> Int
equipCost = sum . map eqCost . equipment

readInput :: IO Input
readInput = return puzzleInput


weaponPicks :: [[Equipment]]
weaponPicks = map pure weapons


armorPicks :: [[Equipment]]
armorPicks = [] : map pure armors


ringPicks :: [[Equipment]]
ringPicks = zero ++ one ++ two
  where zero = [[]]
        one  = map pure rings
        two  = do
          (r1, rs) <- pick rings
          (r2, _)  <- pick rs
          pure [r1, r2]


pick :: [a] -> [(a,[a])]
pick [] = []
pick (a:as) =
  (a,as) : pick as


weapons :: [Equipment]
weapons =
  [ Equipment "dagger" 8 4 0
  , Equipment "shortsword" 10 5 0
  , Equipment "warhammer" 25 6 0
  , Equipment "longsword" 40 7 0
  , Equipment "greataxe" 74 8 0
  ]

armors :: [Equipment]
armors =
  [ Equipment "leather" 13 0 1
  , Equipment "chainmail" 31 0 2
  , Equipment "splintmail" 53 0 3
  , Equipment "bandemail" 75 0 4
  , Equipment "platemail" 102 0 5
  ]

rings :: [Equipment]
rings =
  [ Equipment "damage +1" 25 1 0
  , Equipment "damage +2" 50 2 0
  , Equipment "damage +3" 100 3 0
  , Equipment "defense +1" 20 0 1
  , Equipment "defense +2" 40 0 2
  , Equipment "defense +3" 80 0 3
  ]
