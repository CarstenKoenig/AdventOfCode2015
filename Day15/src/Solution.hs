module Solution (readInput, part1, part2)  where

import Data.Function (on)
import Data.List (maximumBy)

----------------------------------------------------------------------
-- data and types

type Input = [Ingredient]


data Ingredient =
  Ingredient
  { name        :: String
  , capacity    :: Int
  , durability :: Int
  , flavor      :: Int
  , texture     :: Int
  , calories    :: Int
  } deriving Show


type Recipe = [(Int, Ingredient)]

----------------------------------------------------------------------
-- solution

part1 :: Input -> Int
part1 = score . bestRecipe 100


part2 :: Input -> Int
part2 = score . bestRecipeWithCalories 100 500


bestRecipeWithCalories :: Int -> Int -> Input -> Recipe
bestRecipeWithCalories total cals =
  maximumBy (compare `on` score) . filter ((== cals) . recipeCalories) . (recipes total)


bestRecipe :: Int -> Input -> Recipe
bestRecipe total = maximumBy (compare `on` score) . (recipes total)


recipeCalories :: Recipe -> Int
recipeCalories = sum . map (\ (n, ing) -> n * calories ing)


score :: Recipe -> Int
score recipe = product [ scoreItem part | part <- [capacity, durability, flavor, texture ]]
  where
    scoreItem getter =
      max 0 $ sum $ map (\ (n,i) -> n * getter i) recipe


recipes :: Int -> Input -> [Recipe]
recipes total ings = [ zip ams ings | ams <- amounts total (length ings) ]


amounts :: Int -> Int -> [[Int]]
amounts total n = go total n
  where
    go rem 1 = [[rem]]
    go rem n = do
      x  <- [0..rem]
      xs <- go (rem - x) (n-1)
      return (x:xs)


----------------------------------------------------------------------
-- input

readInput :: IO Input
readInput = return input


input :: Input
input =
  [ Ingredient "Sprinkles" 5 (-1) 0 0 5
  , Ingredient "PeanutButter" (-1) 3 0 0 1
  , Ingredient "Frosting" 0 (-1) 4 0 6
  , Ingredient "Sugar" (-1) 0 0 2 8
  ]


example :: Input
example =
  [ Ingredient "Butterscotch" (-1) (-2) 6 3 8
  , Ingredient "Cinnamon" 2 3 (-2) (-1) 3
  ]


exampleRecipe :: Recipe
exampleRecipe = zip [44, 56] example
