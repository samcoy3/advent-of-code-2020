module Days.Day17 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text

import Util.Parsers
import Util.Util
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Set.fromList . Map.keys <$> coordinateParser mapper 0
  where
    mapper '#' = Just True
    mapper _ = Nothing

------------ TYPES ------------
-- A typeclass to unify the solutions to both parts
class (Eq p, Ord p) => Point p where
  -- This is used to lift a point from the input into the relevant space
  lift :: (Int, Int) -> p
  -- This is the neighbour function for the relevant point type
  neighbours :: p -> [p]
  -- Given the set of active points, this calculates the set of points that we need to think about for the next round
  consideration :: Set p -> Set p

type Slice = Set (Int, Int)

type Input = Slice

type OutputA = Int

type OutputB = Int

------------ PART A ------------
newtype Point3 = Point3 (Int, Int, Int)
  deriving (Show, Eq, Ord)

instance Point Point3 where
  lift (x, y) = Point3 (x, y, 0)
  consideration activePoints =
    Set.fromList . Map.keys . Map.filter (>= 2) . freq . Set.foldr (++) [] . Set.map neighbours $ activePoints
  neighbours (Point3 (x, y, z)) =
    Point3
      <$> [ (x', y', z')
            | x' <- [x -1 .. x + 1],
              y' <- [y -1 .. y + 1],
              z' <- [z -1 .. z + 1],
              x' /= x || y' /= y || z' /= z
          ]

-- Given the set of currently active points, determines whether a point becomes active or not
evolveSquare :: (Point p) => Set p -> p -> Bool
evolveSquare space s =
  let aliveNeighbours = length $ filter (`elem` space) (neighbours s)
   in if
          | s `elem` space && (aliveNeighbours == 2 || aliveNeighbours == 3) ->
            True
          | s `notElem` space && (aliveNeighbours == 3) ->
            True
          | otherwise -> False

-- Evolves a set of active points, applying the rules simultaneously
evolveSpace :: (Point p) => Set p -> Set p
evolveSpace space =
  Set.filter (evolveSquare space) (consideration space)

partA :: Input -> OutputA
partA = Set.size . foldr1 (.) (replicate 6 evolveSpace) . Set.map (lift @Point3)

------------ PART B ------------
newtype Point4 = Point4 (Int, Int, Int, Int)
  deriving (Eq, Show, Ord)

instance Point Point4 where
  lift (x, y) = Point4 (x, y, 0, 0)
  consideration activePoints =
    Set.fromList . Map.keys . Map.filter (>= 2) . freq . Set.foldr (++) [] . Set.map neighbours $ activePoints
  neighbours (Point4 (x, y, z, w)) =
    Point4
      <$> [ (x', y', z', w')
            | x' <- [x -1 .. x + 1],
              y' <- [y -1 .. y + 1],
              z' <- [z -1 .. z + 1],
              w' <- [w -1 .. w + 1],
              x' /= x || y' /= y || z' /= z || w' /= w
          ]

partB :: Input -> OutputB
partB = Set.size . foldr1 (.) (replicate 6 evolveSpace) . Set.map (lift @Point4)
