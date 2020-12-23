module Days.Day14 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List as L
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Functor (($>))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
-- We choose to parse our input as a list of memory writes with associated masks
-- This makes associating masks with writes a little easier
inputParser :: Parser Input
inputParser = value (Vec.empty)
  where
    -- These are the three things we could see each line
    value mask =
      choice
        [ endOfInput $> [],
          newMask,
          instruction mask
        ]
    -- Changes the "current" mask
    newMask = do
      string "mask = "
      mask <-
        Vec.fromList
          <$> ( count
                  36
                  $ choice
                    [ char 'X' $> Nothing,
                      char '1' $> Just True,
                      char '0' $> Just False
                    ]
              )
      endOfLine
      value mask
    -- Parses a memory write with the "current" mask associated
    instruction mask = do
      string "mem["
      pos <- decimal
      string "] = "
      val <- decimal
      endOfLine
      (:) <$> (return (mask, (pos, val))) <*> value mask

------------ TYPES ------------
type Mask = Vector (Maybe Bool)

type Input = [(Mask, (Int, Int))]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Converts a 36-length vector of 1s and 0s (representing a binary number) to an integer
numberise :: Vector Int -> Int
numberise vec =
  sum . Vec.zipWith (*) vec
    . Vec.fromList
    . reverse
    . L.take 36
    $ fmap (2 ^) [0 ..]

-- Converts a number to a 36-length binary vector which represents it
vectorise :: Int -> Vector Int
vectorise =
  Vec.fromList
    . reverse
    . L.take 36
    . unfoldr (\n -> if n `mod` 2 == 1 then Just (1, n `div` 2) else Just (0, n `div` 2))

-- Apply the bitmask to a value, as described in Part A of the problem
maskValue :: Mask -> Int -> Int
maskValue mask =
  numberise
    . Vec.zipWith (\m v -> maybe v fromEnum m) mask
    . vectorise

partA :: Input -> OutputA
partA =
  sum . Map.elems
    -- Iteratively insert the masked values into memory
    . foldl'
      ( \mem (mask, (pos, val)) ->
          Map.insert
            pos
            (maskValue mask val)
            mem
      )
      Map.empty

------------ PART B ------------
{-
Applies the mask to an integer as described in Part B.
This works by mapping each index of the vector to a list of values, and then sequencing the result.
Sequencing the result creates "all possible paths" through the vector, because we're sequencing into the List monad.
For example (here V == Vec.fromList for brevity):
  sequence $ V [[0], [0,1], [1], [0,1]]
  === [V [0,0,1,0], V [0,0,1,1], V [0,1,1,0], V [0,1,1,1]]
-}
maskPosition :: Mask -> Int -> [Int]
maskPosition mask = fmap numberise . sequence . applyMask mask . vectorise
  where
    applyMask =
      Vec.zipWith
        ( \m v -> case m of
            Just True -> [1]
            Just False -> [v]
            Nothing -> [0, 1]
        )

partB :: Input -> OutputB
partB =
  sum . Map.elems
    -- Outer fold maps over all "write instructions"
    . foldl'
      ( \mem (mask, (pos, val)) ->
          -- Inner fold expands the memory locations that we write to, and writes to them iteratively
          -- A foldr is fine here, we don't care about the order as the memory addresses are all different
          foldr
            ( \pos' ->
                Map.insert pos' val
            )
            mem
            (maskPosition mask pos)
      )
      Map.empty
