module Solution (readInput, part1, part2)  where

import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Control.Monad.State as St

type Game a = St.State Fight a


data Fight =
  Fight
  { wizzard  :: Character
  , boss     :: Character
  , manaCast :: Mana
  }

data Character =
  Character
  { hitpoints :: Hitpoints
  , mana      :: Mana
  , damage    :: Damage
  , armor     :: Armor
  , effects   :: M.Map String Effect
  } deriving Show


data Effect =
  Effect
  { name     :: String
  , duration :: Duration
  , roundEff :: Character -> Character
  , endEff   :: Character -> Character
  }


type Duration = Int
type Hitpoints = Int
type Mana = Int
type Damage = Int
type Armor = Int


instance Show Effect where
  show eff = name eff ++ " [" ++ show (duration eff) ++ "]"


castMagicMissle :: Game ()
castMagicMissle =
  St.modify (useMana 53 . damageBoss 4)


castDrain :: Game ()
castDrain =
  St.modify (useMana 73 . healsHero 2 . damageBoss 2)


castShield :: Game ()
castShield =
  St.modify (useMana 113 . modifyHeroArmor (+ 7) . addHeroEffect shieldEffect )
  where shieldEffect = Effect "shield" 6 id (modifyArmor (subtract 7))


castPoison :: Game ()
castPoison =
  St.modify (useMana 173 . addBossEffect poisonEffect )
  where poisonEffect = Effect "poison" 6 (modifyHitpoints (subtract 3)) id


castRecharge :: Game ()
castRecharge =
  St.modify (useMana 229 . addHeroEffect rechargeEffect )
  where rechargeEffect = Effect "recharge" 5 (modifyMana (+ 101)) id


hitHero :: Game ()
hitHero = do
  bossDmg <- St.gets (damage . boss)
  heroArm <- St.gets (armor . wizzard)
  St.modify (damageHero $ max 1 (bossDmg - heroArm))


heroRound :: Game () -> Game ()
heroRound spell = do
  St.modify (\f -> f { wizzard = applyEffects (wizzard f) })
  spell


bossRound :: Game ()
bossRound = do
  St.modify (\f -> f { boss = applyEffects (boss f) })
  hitHero


applyEffects :: Character -> Character
applyEffects ch = foldl' applyEffect ch (M.elems $ effects ch)


applyEffect :: Character -> Effect -> Character
applyEffect ch eff =
  let eff' = decDuration eff
  in updateEff eff' . applyAfter eff' $ applyRound eff' ch
  where
    decDuration eff = eff { duration = duration eff - 1 }
    applyRound e = roundEff e
    applyAfter e = endEff e
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


type Input = ()


part1 :: Input -> ()
part1 inp = ()


part2 :: Input -> ()
part2 inp = ()


readInput :: IO Input
readInput = return ()
