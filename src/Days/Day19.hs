module Days.Day19 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void

import Control.Applicative.Combinators ((<|>))
import Util.Parsers (around)
import Control.Monad
import Debug.Trace (traceShow, traceShowId, trace)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (,) <$> (rules <* skipSpace) <*> strings
  where
    rule = do
      index <- decimal
      string ": "
      subRules <-
        (Character <$> (char '"' >> letter <* char '"'))
          <|> (SubRules <$> (eitherP (decimal `around` (char ' ')) decimal) `sepBy` string " | ")
      return (index, subRules)
    rules = Map.fromList <$> rule `sepBy` endOfLine
    strings = many1 letter `sepBy` endOfLine

------------ TYPES ------------
data Rule
  = Character Char
  | SubRules [Either (Int, Int) Int]
  deriving (Eq, Show)

type RuleMap = Map Int Rule

type Input = (RuleMap, [String])

type OutputA = Int

type OutputB = Void

------------ PART A ------------
traceShowIdWithContext :: (Show a, Show b) => a -> b -> b
traceShowIdWithContext context result = trace (show context ++ "\t" ++ show result) result

doesMatchRule :: RuleMap -> Rule -> String -> Bool
doesMatchRule map rule str =
  -- traceShowIdWithContext (rule, str) $
  case rule of
    Character c -> pure c == str
    SubRules rs -> any doesMatchRule' rs
      where
        doesMatchRule' :: Either (Int, Int) Int -> Bool
        doesMatchRule' (Left (r1, r2)) =
          any (\(s1, s2) -> doesMatchRule map (map Map.! r1) s1 && doesMatchRule map (map Map.! r2) s2) $ allPossibleBreaks (map Map.! r1) str
        doesMatchRule' (Right r) = doesMatchRule map (map Map.! r) str
        allPossibleBreaks :: Rule -> String -> [(String, String)]
        allPossibleBreaks r s =
          -- traceShowIdWithContext (rs, r, s) $
          zipWith splitAt (lengths map r) (repeat str)

lengths :: RuleMap -> Rule -> [Int]
lengths _ (Character _) = [1]
lengths map (SubRules rs) = foldr (++) [] (fmap length' rs)
  where
    length' :: Either (Int, Int) Int -> [Int]
    length' (Left (r1, r2)) = nub $ (+) <$> (lengths map (map Map.! r1)) <*> (lengths map (map Map.! r2))
    length' (Right r) = lengths map (map Map.! r)

partA :: Input -> OutputA
partA (ruleMap, strings) = length . filter (doesMatchRule ruleMap (ruleMap Map.! 0)) $ strings

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
