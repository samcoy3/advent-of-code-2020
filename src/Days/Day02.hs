module Days.Day02 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Bits (xor)
import qualified Data.Map.Strict as Map
import qualified Util.Util as U
import Util.Parsers

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = ruleAndPassword `sepBy` endOfLine
  where
    ruleAndPassword = do
      (min, max) <- decimal `around` char '-'
      skipSpace
      givenLetter <- letter
      string ": "
      password <- many1 letter
      return (PasswordRule {..}, password)

------------ TYPES ------------
type Password = String

data PasswordRule = PasswordRule
  { min :: Int,
    max :: Int,
    givenLetter :: Char
  }
  deriving (Show)

type Input = [(PasswordRule, Password)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = length . filter scorePassword
  where
    scorePassword (PasswordRule {..}, password) =
      let countOfGiven = Map.findWithDefault 0 givenLetter (U.freq password)
       in countOfGiven >= min && countOfGiven <= max

------------ PART B ------------
partB :: Input -> OutputB
partB = length . filter scorePassword
  where
    scorePassword (PasswordRule {..}, password) =
      (password !! (min -1) == givenLetter) `xor` (password !! (max -1) == givenLetter)
