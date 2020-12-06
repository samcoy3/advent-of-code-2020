module Days.Day06 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import qualified Data.Set as Set

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (many1 letter `sepBy` endOfLine) `sepBy` (string "\n\n")

------------ TYPES ------------
type Input = [[String]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . fmap (length . nub . concat)

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . fmap (Set.size . foldr1 Set.intersection . fmap Set.fromList)
