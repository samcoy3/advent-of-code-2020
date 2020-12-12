module Days.Day11 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text

import Util.Parsers
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
-- We detect Floor here because we need it for the visibility checking in B
-- We could have used bounds checking, but decided not to
inputParser :: Parser Input
inputParser = coordinateParser mapper 0
  where
    mapper 'L' = Just Empty
    mapper '.' = Just Floor
    mapper _ = Nothing

------------ TYPES ------------
data SeatStatus = Empty | Occupied | Floor deriving (Eq, Show)

type NeighbourhoodFunction = (Int, Int) -> [(Int, Int)]

type Input = Map (Int, Int) SeatStatus

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Iterates a function until a fixed-point
iterateUntilStable :: (Eq a) => (a -> a) -> a -> a
iterateUntilStable f x
  | f x == x = x
  | otherwise = iterateUntilStable f (f x)

neighboursA :: NeighbourhoodFunction
neighboursA (x, y) =
  [ (a, b)
    | a <- [x -1 .. x + 1],
      b <- [y -1 .. y + 1],
      a /= x || b /= y
  ]

evolveSeat ::
  Map (Int, Int) SeatStatus ->
  NeighbourhoodFunction ->
  Int -> -- Threshold (4 for A, 5 for B)
  (Int, Int) ->
  SeatStatus ->
  SeatStatus
evolveSeat m nhood threshold seat status =
  let neighbours = nhood seat
      numberOfOccupiedNeighbours =
        length . filter (== Occupied)
          . fmap (\s -> Map.findWithDefault Empty s m)
          $ neighbours
   in if
          | status == Empty && numberOfOccupiedNeighbours == 0 ->
            Occupied
          | status == Occupied && numberOfOccupiedNeighbours >= threshold ->
            Empty
          | otherwise -> status

-- This function applies the rules given in the question until it's stable, then returns the number of occupied seats
evolveSeatingArea :: Map (Int, Int) SeatStatus -> NeighbourhoodFunction -> Int -> Int
evolveSeatingArea seats nfunc thresh =
  -- We define a Map here to lower the number of lookups during the execution of our function
  let neighbourMap = Map.fromList $ fmap (\s -> (s,) $ nfunc s) (Map.keys seats)
   in Map.size
        . Map.filter (== Occupied)
        $ iterateUntilStable (\m -> Map.mapWithKey (evolveSeat m (neighbourMap Map.!) thresh) m) seats

partA :: Input -> OutputA
partA input = evolveSeatingArea input neighboursA 4

------------ PART B ------------
-- This function returns a neighbourhood function based on visibility
neighboursB :: Map (Int, Int) SeatStatus -> NeighbourhoodFunction
neighboursB m s =
  let vectors = [(a, b) | a <- [-1 .. 1], b <- [-1 .. 1], a /= 0 || b /= 0]
      -- We define Point addition
      (a, b) <+> (c, d) = (a + c, b + d)
      iterateUntilSeat s vec = case m Map.!? s of
        Just Floor -> iterateUntilSeat (s <+> vec) vec
        Nothing -> Nothing
        _ -> Just s
   in catMaybes $
        zipWith (\v s -> iterateUntilSeat (s <+> v) v) vectors (repeat s)

partB :: Input -> OutputB
partB input =
  evolveSeatingArea input (neighboursB input) 5
