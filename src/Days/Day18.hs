module Days.Day18 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text

import Data.Functor (($>))
import Data.Attoparsec.Combinator
import Control.Applicative.Combinators ((<|>), many, between)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
-- We parse the expression tree in two different ways, using lookAhead
inputParser :: Parser Input
inputParser = (,) <$> (lookAhead (expressionA `sepBy` endOfLine)) <*> (expressionB `sepBy` endOfLine)
  where
    expressionA = do
      let operator = char '+' $> Add <|> char '*' $> Mul
          -- A term is any expression which is a "subtree" in its own right
          termA =
            (char '(' >> (Bracketed <$> expressionA) <* char ')')
              <|> Value <$> decimal
      -- We note that we start with a value, and then zero or more pairs of (operator,term)
      startingTerm <- termA
      otherTerms <- many $ (,) <$> (between space space operator) <*> termA
      -- We can just fold from the left over the operators (because there is no precedence except left->right)
      return $ foldl' (\t (op, t') -> op t t') startingTerm otherTerms

    expressionB = do
      -- An "addition expression" is one entirely comprised of terms and additions.
      let additionExpression = do
            startingTerm <- termB
            otherTerms <- many $ (,) <$> (string " + " $> Add) <*> termB
            -- We fold over these as before (as we only have addition, we again have no precedence issues)
            return $ foldl' (\term (op, term') -> op term term') startingTerm otherTerms
          -- This is defined separately to termA because it needs to recurse to expressionB's
          termB = (char '(' >> (Bracketed <$> expressionB) <* char ')') <|> Value <$> decimal
      -- We parse a series of "addition expressions" separated by multiplications
      addTerms <- additionExpression `sepBy1` (string " * ")
      -- We then fold over multiplication, because we've already folded over addition so this guarantees that multiplication will be higher in the tree
      -- N.B. We have a guarantee that the list here is non-empty - a single term is an addition expression, so we need at least one to the left of a *
      return $ foldl1' Mul addTerms

------------ TYPES ------------
data Expr where
  Value :: Int -> Expr
  Bracketed :: Expr -> Expr
  Mul :: Expr -> Expr -> Expr
  Add :: Expr -> Expr -> Expr
  deriving (Show)

type Input = ([Expr], [Expr])

type OutputA = Int

type OutputB = Int

------------ PART A ------------
eval :: Expr -> Int
eval (Value n) = n
eval (Bracketed e) = eval e
eval (Mul e f) = (eval e) * (eval f)
eval (Add e f) = (eval e) + (eval f)

partA :: Input -> OutputA
partA = sum . fmap eval . fst

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . fmap eval . snd
