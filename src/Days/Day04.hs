module Days.Day04 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (pack)

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Control.Applicative
import Control.Monad (void, guard)
import Data.Either (isRight)

import Data.Function (on)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
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
-- Given two parsers, provides a parser that checks if a number is in some bounds.
inRangeInclusive :: Int -> Int -> Int -> Parser ()
inRangeInclusive min max x = guard (x >= min && x <= max)

{-
In this part, we:
* Create a list of the values of the fields, in alphabetic order ("byr", "ecl"...)
* Create a list of parsers for these fields
* Zip them together with parser application, giving us a list of Either String () values
* Sequence this list, giving us an Either String [()]
* If this value isRight (i.e. Right [()]), then all parsers have succeeded and our passport is valid.
N.B. "void" discards the value of a Parser, giving us a Parser () (it's of type Parser a -> Parser ())
-}
partB :: Input -> OutputB
partB = length . filter validPassport . (fmap (Map.delete "cid")) . filter correctFields
  where
    validPassport p =
      let fieldsInOrder = fmap pack . fmap snd . sortBy (compare `on` fst) . Map.toList $ p
          conditions =
            [ decimal >>= inRangeInclusive 1920 2002,
              void (choice (fmap string ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])),
              decimal >>= inRangeInclusive 2020 2030,
              char '#' >> void (count 6 (satisfy $ inClass "0-9a-f")),
              void
                ( choice
                    [ decimal >>= inRangeInclusive 150 195 >> string "cm",
                      decimal >>= inRangeInclusive 59 76 >> string "in"
                    ]
                ),
              decimal >>= inRangeInclusive 2010 2020,
              void (count 9 digit)
            ]
       in isRight
            . sequence
            $ zipWith (\p f -> parseOnly (p <* endOfInput) f) conditions fieldsInOrder
