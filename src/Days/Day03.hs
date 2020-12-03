module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List as L
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

import Util.Parsers (coordinateParser)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser (flip lookup [('#', Tree), ('.', Open)]) 0

------------ TYPES ------------
data Square = Tree | Open deriving (Eq, Show)

type Input = Map (Int, Int) Square

type OutputA = Int

type OutputB = Int

------------ PART A ------------
slope :: Input -> Int -> Int -> Int
slope m x y =
  -- Need to add one to the width because the map is zero-indexed
  let width = (+ 1) . fst . maximum . Map.keys $ m
      xs = fmap (`mod` width) [0, x ..]
      coords = zip xs [0, y ..]
   in length
        . filter (== Tree)
        . fmap fromJust
        . L.takeWhile (not . isNothing)
        . fmap (m Map.!?)
        $ coords

partA :: Input -> OutputA
partA m = slope m 3 1

------------ PART B ------------
partB :: Input -> OutputB
partB m =
  product . fmap (uncurry (slope m)) $
    [ (1, 1),
      (3, 1),
      (5, 1),
      (7, 1),
      (1, 2)
    ]
