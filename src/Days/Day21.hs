module Days.Day21 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Ord (comparing)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = food `sepBy` endOfLine
  where
    food = do
      ingredients <- many1 letter `sepBy` space
      string " (contains "
      allergens <- many1 letter `sepBy` string ", "
      char ')'
      return (ingredients, allergens)

------------ TYPES ------------
type Ingredient = String

type Allergen = String

type Input = [([Ingredient], [Allergen])]

type OutputA = Int

type OutputB = String

------------ PART A ------------
-- Given our input, produces a map from allergens to ingredients in which they could possible appear
allergenPossibilityMap :: Input -> Map Allergen (Set Ingredient)
allergenPossibilityMap input =
  foldr
    ( \(ingredients, allergens) map ->
        foldr
          ( \allergen map' ->
              Map.insertWith
                Set.intersection
                allergen
                (Set.fromList ingredients)
                map'
          )
          map
          allergens
    )
    Map.empty
    input

partA :: Input -> OutputA
partA input =
  let allIngredients = concat . fmap fst $ input
      -- All ingredients which could plausibly contain an allergen
      allPossibleAllergens = Set.unions . Map.elems $ allergenPossibilityMap input
   in length $ filter (`notElem` allPossibleAllergens) allIngredients

------------ PART B ------------
-- Given a map from allergens to ingredients which might contain them, this function performs the matching
-- That is, it identifies which ingredients contains which allergens
findAllergens :: Map Allergen (Set Ingredient) -> Map Ingredient Allergen
findAllergens = findAllergens' Map.empty
  where
    findAllergens' accum allergenMap =
      if Map.null allergenMap
        then accum
        else -- We find the allergen which has only one possible ingredient

          let (all, ing') = Map.findMin $ Map.filter ((== 1) . Set.size) allergenMap
              ing = Set.findMin ing'
           in -- We recursively call this function, removing the allergen and ingredient from the map which we still have to sort, and adding them to our accumulator
              findAllergens'
                (Map.insert ing all accum)
                (Map.delete all . Map.map (Set.delete ing) $ allergenMap)

partB :: Input -> OutputB
partB input =
  let allergenMap = findAllergens . allergenPossibilityMap $ input
   in concat
        . intersperse ","
        . fmap fst
        -- Sorting alphabetically by allergen
        . sortBy (comparing snd)
        . Map.toList
        $ allergenMap
