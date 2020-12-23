module Days.Day23 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Char (isDigit)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 (read . pure <$> satisfy isDigit)

------------ TYPES ------------
type Input = [Int]

type OutputA = String

type OutputB = Int

type CupMap = IntMap Int

------------ PART A ------------
-- Our initial list of cups
startingCups :: Input -> (Int, CupMap)
startingCups input =
  (,)
    (head input)
    (Map.fromList $ zip input (tail $ cycle input))

-- Do one step of the cup movement
-- We use an IntMap as our data structure here; it's reasonably quick
-- The idea is that we store a map from cups to the next cup
evolveCups :: (Int, Int) -> (Int, CupMap) -> (Int, CupMap)
evolveCups (min, max) (current, cupMap) =
  let -- c1, c2, and c3 are the three cups we pick up
      -- We need c4 so that we can modify the "linked list"
      c1 = cupMap Map.! current
      c2 = cupMap Map.! c1
      c3 = cupMap Map.! c2
      c4 = cupMap Map.! c3
      destination' n =
        if
            | n < min -> destination' max
            | n `elem` [c1, c2, c3] -> destination' (n -1)
            | otherwise -> n
      destination = destination' (current - 1)
      -- Again, we need the cup after the destination so that we can modify the map appropriately
      afterDestination = cupMap Map.! destination
   in (,)
        c4
        ( Map.insert current c4
            . Map.insert destination c1
            . Map.insert c3 afterDestination
            $ cupMap
        )

-- This prints the cups in the pretty way which Part A requires
finalCups :: CupMap -> String
finalCups cupMap = tail . concat . fmap show . Prelude.take 9 . iterate (cupMap Map.!) $ 1

partA :: Input -> OutputA
partA = finalCups . snd . (!! 100) . iterate (evolveCups (1, 9)) . startingCups

------------ PART B ------------
startingCupsB :: Input -> (Int, CupMap)
startingCupsB input =
  let cupList = input ++ [10 .. 1_000_000]
   in (,)
        (head input)
        (Map.fromList $ zip cupList (tail $ cycle cupList))

partB :: Input -> OutputB
partB input =
  let finalOrder = snd . (!! 10_000_000) . iterate (evolveCups (1, 1_000_000)) . startingCupsB $ input
      star1 = finalOrder Map.! 1
      star2 = finalOrder Map.! star1
   in star1 * star2
