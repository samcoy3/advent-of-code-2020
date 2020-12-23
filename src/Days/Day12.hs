module Days.Day12 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text

import Util.Pair
import Data.Functor
import Control.Monad.State
import Control.Applicative ((<|>))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = instruction `sepBy` endOfLine
  where
    instruction =
      choice
        [ char 'F' >> Forwards <$> decimal,
          Move <$> direction <*> decimal,
          Turn <$> ((*) <$> turnMod <*> decimal)
        ]
    direction =
      choice
        [ char 'N' $> North,
          char 'E' $> East,
          char 'S' $> South,
          char 'W' $> West
        ]
    turnMod = char 'L' $> -1 <|> char 'R' $> 1

------------ TYPES ------------
data Direction = North | East | South | West deriving (Show, Eq, Enum)

-- Note that in this datatype we've already handled the degrees of the turn instruction.
-- Turning left is represented as negative turning right here.
data Instruction
  = Forwards Int
  | Move Direction Int
  | Turn Int
  deriving (Eq, Show)

data Ferry = Ferry
  { location :: Pair Int,
    direction :: Direction
  }

type FerryM = State Ferry

type Input = [Instruction]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Given a direction and an integer, moves a point that far in that direction
moveInDir :: Direction -> Int -> Pair Int -> Pair Int
moveInDir d n (Pair (x, y)) = Pair $ case d of
  North -> (x, y + n)
  East -> (x + n, y)
  South -> (x, y - n)
  West -> (x - n, y)

execInstruction :: Instruction -> FerryM ()
execInstruction (Forwards n) =
  modify (\f -> f {location = moveInDir (direction f) n $ location f})
execInstruction (Move d n) =
  modify (\f -> f {location = moveInDir d n $ location f})
execInstruction (Turn n) =
  -- Uses an Enum conversion to do the heavy lifting, here.
  let notches = n `div` 90
   in modify (\f -> f {direction = toEnum ((fromEnum (direction f) + notches) `mod` 4)})

partA :: Input -> OutputA
partA =
  sum . fmap abs
    . location
    . (flip execState Ferry {location = Pair (0, 0), direction = East})
    . sequence_
    . fmap execInstruction

------------ PART B ------------
data FerryW = FerryW
  { location' :: Pair Int,
    waypoint :: Pair Int
  }

type FerryWM = State FerryW

execInstructionB :: Instruction -> FerryWM ()
execInstructionB (Forwards n) =
  modify (\f -> f {location' = (location' f) <+> ((* n) <$> (waypoint f))})
execInstructionB (Move d n) =
  modify (\f -> f {waypoint = moveInDir d n $ waypoint f})
execInstructionB (Turn n) =
  let notches = (n `div` 90) `mod` 4
      rotateRightOnce (Pair (x, y)) = Pair (y, - x)
      modifier = foldr (.) id (replicate notches rotateRightOnce)
   in modify (\f -> f {waypoint = modifier (waypoint f)})

partB :: Input -> OutputB
partB =
  sum . fmap abs
    . location'
    . (flip execState FerryW {location' = Pair (0, 0), waypoint = Pair (10, 1)})
    . sequence_
    . fmap execInstructionB
