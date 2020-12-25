module Days.Day25 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Maybe

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text

import Util.Parsers
import Debug.Trace (traceShowId)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `around` endOfLine

------------ TYPES ------------
type Input = (Int, Int)

type OutputA = Int

type OutputB = String

------------ PART A ------------
loop :: Int -> Int -> Int
loop subjectNumber n = (n * subjectNumber) `mod` 20_201_227

partA :: Input -> OutputA
partA (x, y) =
  let xIters = traceShowId . fromJust $ findIndex (== x) (iterate (loop 7) 1)
   in foldr1 (.) (replicate xIters (loop y)) $ 1

------------ PART B ------------
partB :: Input -> OutputB
partB = const "Thank you for the star, Rudolph!"
