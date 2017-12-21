module Solution (readInput, part1, part2)  where

import Prelude hiding (log)

import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Graph.AStar
import qualified Data.HashSet as HS
import qualified Data.Hashable as H
import Data.Maybe (fromJust)


data Fight =
  Fight
  { wizzard  :: Character
  , boss     :: Character
  , history  :: [String]
  , manaCast :: Mana
  } deriving Show

instance Eq Fight where
  fa == fb = history fa == history fb

instance Ord Fight where
  fa `compare` fb = history fa `compare` history fb

instance H.Hashable Fight where
  hashWithSalt n fight = H.hashWithSalt n (history fight)


data Character =
  Character
  { hitpoints :: Hitpoints
  , mana      :: Mana
  , damage    :: Damage
  , armor     :: Armor
  , effects   :: M.Map String Effect
  } deriving (Show, Eq, Ord)

instance H.Hashable Character where
  hashWithSalt n (Character hp mn dmg arm eff) = H.hashWithSalt n (hp, mn, dmg, arm, M.toList eff)


data Effect =
  Effect
  { name     :: String
  , duration :: Duration
  , roundEff :: Character -> Character
  , endEff   :: Character -> Character
  }

instance Show Effect where
  show eff = name eff ++ " [" ++ show (duration eff) ++ "]"

instance Eq Effect where
  e1 == e2 = name e1 == name e2 && duration e1 == duration e2

instance Ord Effect where
  e1 `compare` e2 = (name e1, duration e1) `compare` (name e2, duration e2)

instance H.Hashable Effect where
  hashWithSalt n (Effect na du _ _) = H.hashWithSalt n (na, du)



data Result
  = Won
  | Died
  | BattleOn
  deriving (Show, Eq)


type Duration = Int
type Hitpoints = Int
type Mana = Int
type Damage = Int
type Armor = Int
type Spell = Fight -> Fight


part1 :: Input -> Int
part1 fight =
  let (Just winPath) = findWin fight
  in manaCast $ last winPath


part2 :: Input -> ()
part2 inp = ()


test :: Fight
test = Fight wz bs [] 0
  where wz = Character 10 250 0 0 M.empty
        bs = Character 14 0   8 0 M.empty

start :: Fight
start = Fight wz bs [] 0
  where wz = Character 50 500 0 0 M.empty
        bs = Character 58 0   9 0 M.empty


findWin :: Fight -> Maybe [Fight]
findWin fight = aStar (HS.fromList . possiblePlays) dist heur hasWon fight
  where dist ff ft = manaCast ft - manaCast ff
        hasWon f = outcome f == Won
        heur f = let hp = hitpoints (boss f) in hp * 9


possiblePlays :: Fight -> [Fight]
possiblePlays fight0 = do
  let fight = damageHero 1 fight0
  case outcome fight of
    Died -> []
    _ -> do
      let fight' = effectsRound fight
      case outcome fight' of
        Won -> return fight'
        _   -> do
          spell <- availableSpells fight'
          let fight'' = heroRound spell fight'
          case outcome fight'' of
            Won -> return fight''
            _   -> do
              let fight''' = effectsRound fight''
              case outcome fight''' of
                Won  -> return fight'''
                _  -> do
                  let fight'''' = bossRound fight'''
                  case outcome fight'''' of
                    Died -> []
                    _ -> return fight''''


availableSpells :: Fight -> [Spell]
availableSpells fight =
  let mn = mana $ wizzard fight
      hasShield = "shield" `M.member` effects (wizzard fight)
      hasPoison = "poison" `M.member` effects (boss fight)
      hasRecharge = "recharge" `M.member` effects (wizzard fight)
  in (if mn >= 53 then [ castMagicMissle ] else [])
     ++ (if mn >= 73 then [ castDrain ] else [])
     ++ (if mn >= 113 && not hasShield then [ castShield ] else [])
     ++ (if mn >= 173 && not hasPoison then [ castPoison ] else [])
     ++ (if mn >= 229 && not hasRecharge then [ castRecharge ] else [])


stats :: Fight -> String
stats fight = "Wizzard: HP [" ++ (show . hitpoints $ wizzard fight)
              ++ "], Mana [" ++ (show . mana $ wizzard fight)
              ++ "] / Boss: HP [" ++ (show . hitpoints $ boss fight)
              ++ "] / " ++ show (history fight)

outcome :: Fight -> Result
outcome fight =
  let heroHp = hitpoints $ wizzard fight
      bossHp = hitpoints $ boss fight
  in if bossHp <= 0
     then Won
     else if heroHp <= 0
          then Died
          else BattleOn


effectsRound :: Fight -> Fight
effectsRound fight =
  fight { wizzard = applyEffects (wizzard fight)
        , boss    = applyEffects (boss fight)
        }


heroRound :: Spell -> Fight -> Fight
heroRound spell = spell


bossRound :: Fight -> Fight
bossRound = hitHero


castMagicMissle :: Fight -> Fight
castMagicMissle = log "cast magic missle" . useMana 53 . damageBoss 4


castDrain :: Fight -> Fight
castDrain = log "cast drain" . useMana 73 . healsHero 2 . damageBoss 2


castShield :: Fight -> Fight
castShield = log "cast shield" . useMana 113 . modifyHeroArmor (+ 7) . addHeroEffect shieldEffect
  where shieldEffect = Effect "shield" 6 id (modifyArmor (subtract 7))


castPoison :: Fight -> Fight
castPoison = log "cast poison" . useMana 173 . addBossEffect poisonEffect
  where poisonEffect = Effect "poison" 6 (modifyHitpoints (subtract 3)) id


castRecharge :: Fight -> Fight
castRecharge = log "cast recharge" . useMana 229 . addHeroEffect rechargeEffect
  where rechargeEffect = Effect "recharge" 5 (modifyMana (+ 101)) id


hitHero :: Fight -> Fight
hitHero fight =
  let bossDmg = damage $ boss fight
      heroArm = armor  $ wizzard fight
  in log "boss hits hero" $ damageHero (max 1 $ bossDmg - heroArm) fight


log :: String -> Fight -> Fight
log text fight = fight { history = history fight ++ [text] }

applyEffects :: Character -> Character
applyEffects ch = foldl' applyEffect ch (M.elems $ effects ch)


applyEffect :: Character -> Effect -> Character
applyEffect ch eff =
  let eff' = decDuration eff
  in updateEff eff' . applyAfter eff' $ applyRound eff' ch
  where
    decDuration eff = eff { duration = duration eff - 1 }
    applyRound e = roundEff e
    applyAfter e = if duration e <= 0 then endEff e else id
    updateEff e c
      | duration e <= 0 = removeEffect (name e) c
      | otherwise       = updateEffect e c


damageHero :: Damage -> Fight -> Fight
damageHero dmg fg = fg { wizzard = modifyHitpoints (subtract dmg) (wizzard fg) }


damageBoss :: Damage -> Fight -> Fight
damageBoss dmg fg = fg { boss = modifyHitpoints (subtract dmg) (boss fg) }


healsHero :: Hitpoints -> Fight -> Fight
healsHero hp fg = fg { wizzard = modifyHitpoints (+ hp) (wizzard fg) }


modifyHeroArmor :: (Armor -> Armor) -> Fight -> Fight
modifyHeroArmor modAr fg = fg { wizzard = modifyArmor modAr (wizzard fg) }


addHeroEffect :: Effect -> Fight -> Fight
addHeroEffect eff fg = fg { wizzard = addEffect eff (wizzard fg) }


addBossEffect :: Effect -> Fight -> Fight
addBossEffect eff fg = fg { boss = addEffect eff (boss fg) }


useMana :: Mana -> Fight -> Fight
useMana mn fg = fg { manaCast = manaCast fg + mn
                   , wizzard  = modifyMana (subtract mn) (wizzard fg)
                   }


modifyMana :: (Mana -> Mana) -> Character -> Character
modifyMana modMn ch = ch { mana = modMn (mana ch) }


modifyHitpoints :: (Hitpoints -> Hitpoints) -> Character -> Character
modifyHitpoints modHp ch = ch { hitpoints = modHp (hitpoints ch) }


modifyArmor :: (Armor -> Armor) -> Character -> Character
modifyArmor modAr ch = ch { armor = modAr (armor ch) }


addEffect :: Effect -> Character -> Character
addEffect eff ch = ch { effects = M.insertWith (flip const) (name eff) eff (effects ch) }


removeEffect :: String -> Character -> Character
removeEffect effName ch = ch { effects = M.delete effName (effects ch) }


updateEffect :: Effect -> Character -> Character
updateEffect eff ch = ch { effects = M.insert (name eff) eff (effects ch) }


type Input = Fight


readInput :: IO Input
readInput = return start
