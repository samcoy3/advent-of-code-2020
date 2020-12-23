module Days.Day04 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (pack)

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Control.Applicative
import Control.Monad (void, guard)
import Data.Either (isRight)

import Data.Function (on)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = passport `sepBy` (string "\n\n")
  where
    passport =
      Map.fromList <$> do
        entry `sepBy` space -- Note that "space" parses both spaces and newlines (and tabs, etc)
    entry =
      (,)
        <$> (many1 letter)
        <*> ((char ':') *> many1 (letter <|> digit <|> char '#'))

------------ TYPES ------------
-- This is not an ideal input format given that we need to do things with the strings in Part B
-- However we can just pack them into Text and parse them again, which is what we do.
type Input = [Map String String]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
{-
We check that if we delete "cid" from the list of keys (a no-op if it isn't present),
  then we end up with the seven mandatory keys.
Note that this protects against the problem of rogue keys appearing in the input.
-}
correctFields :: Map String String -> Bool
correctFields p =
  (Set.delete "cid" (Set.fromList . Map.keys $ p))
    == Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

partA :: Input -> OutputA
partA = length . filter correctFields

------------ PART B ------------
-- Given two ints, provides a parser that checks if a number is between them.
inRangeInclusive :: Int -> Int -> Int -> Parser ()
inRangeInclusive min max x = guard (x >= min && x <= max)

-- A map from the fields of the passport to parsers that succeed if and only if the value in that field is valid.
-- N.B. "void" discards the output type of a parser, i.e. it's a function from Parser a -> Parser ()
passportValidators :: Map String (Parser ())
passportValidators =
  Map.fromList
    . zip ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    $ [ decimal >>= inRangeInclusive 1920 2002,
        decimal >>= inRangeInclusive 2010 2020,
        decimal >>= inRangeInclusive 2020 2030,
        void
          ( choice
              [ decimal >>= inRangeInclusive 150 195 >> string "cm",
                decimal >>= inRangeInclusive 59 76 >> string "in"
              ]
          ),
        char '#' >> void (count 6 (satisfy $ inClass "0-9a-f")),
        void (choice (fmap string ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])),
        void (count 9 digit)
      ]

partB :: Input -> OutputB
partB = length . filter validPassport . filter correctFields
  where
    {-
    In this predicate, we:
    * Combine the passport with the passport validation map, applying the parser to the relevant value. This gives us a Map String (Either String ())
    * Sequence this result, giving us an Either String  (Map String ()), which will be successful if and only if all fields parsed successfully.
    * Check if this value isRight (i.e. is of type Right (Map String ())).
    -}
    validPassport =
      isRight
        . sequence
        . Map.intersectionWith
          (\parser value -> parseOnly (parser <* endOfInput) (pack value))
          passportValidators
