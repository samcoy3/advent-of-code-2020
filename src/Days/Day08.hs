module Days.Day08 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text

import Control.Monad.State
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Vec.fromList <$> instruction `sepBy` endOfLine
  where
    instruction =
      choice
        [ string "acc " >> Acc <$> (signed decimal),
          string "jmp " >> Jmp <$> (signed decimal),
          string "nop " >> Noop <$> (signed decimal)
        ]

------------ TYPES ------------
data Instruction = Acc Int | Jmp Int | Noop Int deriving (Show, Eq)

data EndCondition = Loop | Terminate deriving (Show, Eq)

data GamesConsole = GamesConsole
  { accumulator :: Int, -- The accumulator value
    program :: Vector Instruction, -- Our program
    currentPos :: Int, -- Our current position in the vector of programs
    prevPos :: Set Int -- A set containing our previous positions in the program
  }

type ConsoleM = State GamesConsole

type Input = Vector Instruction

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- A stateful computation that runs our games console program.
runProgram :: ConsoleM EndCondition
runProgram = do
  -- Add our current position to our set of previous positions
  pos <- gets currentPos
  modify (\c -> c {prevPos = Set.insert pos $ prevPos c})

  -- Get and perform the instruction
  program <- gets (program)
  let instruction = program Vec.! pos
  case instruction of
    Noop _ -> return ()
    -- Jumps slightly off so that the following increment makes it correct
    Jmp x -> modify (\c -> c {currentPos = currentPos c + x - 1})
    Acc x -> modify (\c -> c {accumulator = accumulator c + x})

  -- Increments the current position
  modify (\c -> c {currentPos = currentPos c + 1})
  newPos <- gets currentPos
  prevPos <- gets prevPos

  -- Checks termination conditions
  if
      | Set.member newPos prevPos -> return Loop
      | newPos >= (Vec.length program) -> return Terminate
      | otherwise -> runProgram

partA :: Input -> OutputA
partA input =
  accumulator $ -- We're just assuming it will exit with Loop - the questions says it will
    execState
      runProgram
      GamesConsole
        { accumulator = 0,
          program = input,
          prevPos = Set.empty,
          currentPos = 0
        }

------------ PART B ------------
-- Flips the instruction at the given index, if it can be flipped.
flipInstruction :: Input -> Int -> Maybe Input
flipInstruction prog index = case prog Vec.! index of
  Acc _ -> Nothing
  Jmp x -> Just $ prog Vec.// [(index, Noop x)]
  Noop x -> Just $ prog Vec.// [(index, Jmp x)]

-- We get a list of programs with each instruction flipped, then run each of them.
-- We then find the one which terminates properly and read its accumulator.
partB :: Input -> OutputB
partB input =
  accumulator
    . snd
    . fromJust
    . find ((== Terminate) . fst)
    . fmap (runState runProgram)
    . fmap (\prog -> GamesConsole {accumulator = 0, prevPos = Set.empty, currentPos = 0, program = prog})
    . catMaybes
    . fmap (flipInstruction input)
    $ [0 .. (Vec.length input - 1)]
