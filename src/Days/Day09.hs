module Days.Day09 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List as L
import Data.Maybe

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` endOfLine

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA =
  (!! 25)
    . fromJust
    . find
      ( \ls ->
          ls !! 25
            `notElem` [ x + y
                        | x <- (L.take 25 ls),
                          y <- (L.take 25 ls),
                          x /= y
                      ]
      )
    . tails

------------ PART B ------------
partB :: Input -> OutputB
partB input =
  let target = partA input
   in (\ls -> minimum ls + maximum ls)
        . fromJust
        . find (\ls -> sum ls == target)
        . concatMap tails
        . inits
        $ input
