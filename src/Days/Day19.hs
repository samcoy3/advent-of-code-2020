module Days.Day19 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text

import Control.Applicative.Combinators ((<|>), between)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (,) <$> (rules <* skipSpace) <*> strings
  where
    rule = do
      index <- decimal
      string ": "
      subRules <-
        (Character <$> (between (char '"') (char '"') letter))
          <|> (SubRules <$> (decimal `sepBy` char ' ') `sepBy` string " | ")
      return (index, subRules)
    rules = Map.fromList <$> rule `sepBy` endOfLine
    strings = many1 letter `sepBy` endOfLine

------------ TYPES ------------
-- A rule is either a character (the "leaf");
-- or it's a list of series of rules
data Rule
  = Character Char
  | SubRules [[Int]]
  deriving (Eq, Show)

type RuleMap = Map Int Rule

type Input = (RuleMap, [String])

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Tests if a string matches a rule with a given index
doesMatchRule :: RuleMap -> Int -> String -> Bool
doesMatchRule map index str =
  let rule = map Map.! index
   in case rule of
        Character c -> pure c == str
        -- We test here if any of the choice of rules is matched
        SubRules rs -> any (doesMatchRule' str) rs
          where
            -- This function tests whether a given series of rules matches a string
            -- It uses a recursive approach, breaking the string up smartly at each stage
            doesMatchRule' str' (r1 : rs) =
              any (\(s1, s2) -> doesMatchRule map r1 s1 && (doesMatchRule' s2) rs) $ allPossibleBreaks r1 str'
            doesMatchRule' "" [] = True
            doesMatchRule' _ _ = False
            -- We split a string based on the leftmost rule: we only split it in places such that the leftmost rule can generate a string of the given length
            allPossibleBreaks r s =
              zipWith splitAt (lengths map (length s) r) (repeat s)

-- Calculates the possible lengths of strings which satisfy a given rule
-- WARNING: This probably won't work on the general case, but it does work if you only have one "recursive" path
-- We take a "cap" so that in the case where the list could be infinite, it's capped at a value
lengths :: RuleMap -> Int -> Int -> [Int]
lengths map cap r =
  let rule = map Map.! r
   in nub $ case rule of
        Character _ -> [1]
        SubRules rs -> foldr (++) [] (fmap lengths' rs)
          where
            -- lengths' calculates the lengths of strings which satisfy a given series of rules
            lengths' :: [Int] -> [Int]
            lengths' rss@(r' : rs) =
              -- This is the dodgy line: I don't think that the then clause will always work, but because our input is nice, it does
              if r `elem` rss
                then [0, (head $ lengths' (delete r rss)) .. cap]
                else nub $ (+) <$> (lengths map cap r') <*> (lengths' rs)
            lengths' [] = [0]

partA :: Input -> OutputA
partA (ruleMap, strings) = length . filter (doesMatchRule ruleMap 0) $ strings

------------ PART B ------------
partB :: Input -> OutputB
partB (ruleMap, strings) = length . filter (doesMatchRule ruleMap' 0) $ strings
  where
    ruleMap' =
      Map.insert 8 (SubRules [[42], [42, 8]])
        . Map.insert 11 (SubRules [[42, 31], [42, 11, 31]])
        $ ruleMap
