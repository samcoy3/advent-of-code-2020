module Days.Day05 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (many1 letter) `sepBy` endOfLine

------------ TYPES ------------
type Input = [String]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- We note that the seat representation is basically binary, so we convert to it in a standard way.
codeToSeatID :: String -> Int
codeToSeatID =
  foldl'
    ( \acc x ->
        acc * 2
          + (if x `elem` ("BR" :: String) then 1 else 0)
    )
    0

partA :: Input -> OutputA
partA = maximum . fmap codeToSeatID

------------ PART B ------------
partB :: Input -> OutputB
partB = findSeat . sort . fmap codeToSeatID
  where
    findSeat sids =
      head $
        [ y
          | y <- [minimum sids .. maximum sids],
            y `notElem` sids
        ]
