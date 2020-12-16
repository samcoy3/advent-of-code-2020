module Days.Day16 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text

import Util.Parsers
import Control.Applicative ((<|>))
import Data.Monoid
import Data.Ord
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (,) <$> (Map.fromList <$> condition `sepBy1` endOfLine) <*> tickets
  where
    condition = do
      field <- many1 (letter <|> space)
      string ": "
      (low1, high1) <- decimal `around` char '-'
      string " or "
      (low2, high2) <- decimal `around` char '-'
      return (field, (\val -> (val <= high1 && val >= low1) || (val <= high2 && val >= low2)))
    tickets = do
      count 2 endOfLine
      string "your ticket:"
      endOfLine
      ourTicket <- decimal `sepBy1` char ','
      count 2 endOfLine
      string "nearby tickets:"
      endOfLine
      otherTickets <- (decimal `sepBy1` char ',') `sepBy1` endOfLine
      return $ ourTicket : otherTickets

------------ TYPES ------------
-- My skeleton needs a Show instance for the input type, which is very sad.
instance Show (Int -> Bool) where
  show = const "A function"

-- We take the input as a 2-tuple containing:
--  - A map from field names to predicates over values; and
--  - A list of tickets (with ours at the front)
type Input = (Map String (Int -> Bool), [[Int]])

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA (conditions, tickets) =
  let -- We make a huge mega-condition that only passes if all the conditions do
      condition = mconcat . fmap (Any .) . Map.elems $ conditions
   in -- We find the values that violate the condition and add them, as required
      sum . filter (not . getAny . condition) . concat $ tickets

------------ PART B ------------
partB :: Input -> OutputB
partB (conditions, tickets) =
  let -- We find the tickets such that no entries violate any of the conditions
      validTickets = filter (all ((getAny .) . mconcat . fmap (Any .) . Map.elems $ conditions)) tickets
      -- We transpose them, so we have a list of columns containing what we know to be valid values
      columns = transpose validTickets
      -- This is a function: it takes a column and returns all the conditions which could satisfy it
      matchingCondition column = Map.keys . Map.filter (\condition -> all condition column) $ conditions
      -- We note here that we don't yet have a unique assignment of fields to columns!
      -- However, we spot a pattern: one column could only match one field, another can match one of two fields, another one of three, and so on.
      -- To reduce this down, we:
      --  - Sort the possible columns by number of fields they could be
      --  - Perform the zip in actualColumns, where we subtract the previous column's possible fields from the current column's possible fields
      -- Fortunately this gives us a unique answer in this case
      -- We also pair the columns up with our ticket, making it easier to get the answer in the end
      -- actualColumns ends up being basically a map which represents our ticket!
      possibleColumns = sortBy (comparing (length . fst)) $ zip (fmap matchingCondition columns) (head tickets)
      actualColumns =
        zipWith
          (\(possCols, val) impossCols -> (possCols \\ impossCols, val))
          possibleColumns
          ([] : (fmap fst possibleColumns))
   in product . fmap snd . filter (("departure" `isPrefixOf`) . head . fst) $ actualColumns
