module Days.Day07 where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text

import Data.Functor
import Control.Applicative
import Util.Parsers (around)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Map.fromList <$> rule `sepBy` endOfLine
  where
    colour = do
      (adj, col) <- many1 letter `around` space
      return (adj ++ " " ++ col)
    rule = do
      container <- colour
      string " bags contain "
      rules <-
        choice
          [ string "no other bags" $> [],
            bag `sepBy1` (string ", ")
          ]
      char '.'
      return (container, rules)
    bag = do
      quant <- decimal
      space
      col <- colour
      space
      string "bags" <|> string "bag"
      return (quant, col)

------------ TYPES ------------
type Colour = String

{-
We represent the rules as a map from colours to a list of int, colour pairs (representing the bags inside the key bag)
A Set seems like the logical container for the values, but apparently rules such as:
  "light red bags contain 1 bright white bag, 2 bright white bags."
are possible.
-}
type BagRules = Map Colour ([(Int, Colour)])

type Input = BagRules

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Gets the valid containers for the specified bad type.
validContainers :: BagRules -> Colour -> [Colour]
validContainers rs col = case validImmediateContainers col of
  [] -> [col]
  -- Gets the current colour along with everythng "up the tree", and then reduces to ensure uniqueness.
  ls -> nub . (col :) . concat . fmap (validContainers rs) $ ls
  where
    -- Gets the keys of the map which contain the colour we're at somewhere in the value.
    validImmediateContainers col =
      Map.keys . Map.filter ((col `elem`) . (fmap snd)) $ rs

partA :: Input -> OutputA
partA input =
  length
    . (delete "shiny gold") -- Need to remove this - it can't contain itself!
    $ validContainers input "shiny gold"

------------ PART B ------------
-- Gets the total number of bags in the current bag. Doesn't count the bag itself.
totalBags :: BagRules -> Colour -> Int
totalBags rs col =
  sum
    . fmap (\(q, c) -> q + (q * totalBags rs c))
    $ rs Map.! col

partB :: Input -> OutputB
partB input = totalBags input "shiny gold"
