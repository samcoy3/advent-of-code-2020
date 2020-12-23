module Days.Day23 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Char (isDigit)
import qualified Data.Vector.Unboxed.Mutable as MVec
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vec

import Control.Monad
import Control.Monad.ST

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 (read . pure <$> satisfy isDigit)

------------ TYPES ------------
type Input = [Int]

type OutputA = String

type OutputB = Int

------------ PART A ------------
-- Runs our cup movements
-- Uses the ST monad and unboxed vectors, so is crazy fast
-- Iters is the number of iterations we perform; startingCups is a list containing our initial state
-- Our vector can be thought of as a map: if value y is stored at index x in the vector, y is directly clockwise from x
runCups :: Int -> [Int] -> Vector Int
runCups iters startingCups = runST $ do
  let min = minimum startingCups
      max = maximum startingCups
  -- We initialise our vector. Note that it's one longer than it needs to be, and index 0 is unused
  cups <-
    Vec.thaw
      . (Vec.// zip startingCups (tail $ cycle startingCups))
      $ Vec.replicate (length startingCups + 1) 0
  let -- Our function which performs one round
      -- Takes the current cup as an argument, and monadically returns the new "current cup"
      doRound current = do
        -- The first four cups after the current one - we need to attach current to c4
        c1 <- cups `MVec.read` current
        c2 <- cups `MVec.read` c1
        c3 <- cups `MVec.read` c2
        c4 <- cups `MVec.read` c3
        let destination' n =
              if
                  | n < min -> destination' max
                  | n `elem` [c1, c2, c3] -> destination' (n -1)
                  | otherwise -> n
            destination = destination' (current - 1)
        -- We need to insert c1, c2, c3 in between destination and afterDestination
        afterDestination <- cups `MVec.read` destination
        MVec.write cups current c4
        MVec.write cups destination c1
        MVec.write cups c3 afterDestination
        -- We return the new "current cup"
        return c4
  -- This fold composes doRound as many times as we need to run it for, and runs it starting at the beginning of our input
  foldr1 (>=>) (replicate iters doRound) $ head startingCups
  Vec.freeze cups

getAAnswer :: Vector Int -> String
getAAnswer cupMap = tail . concat . fmap show . Prelude.take 9 . iterate (cupMap Vec.!) $ 1

partA :: Input -> OutputA
partA = getAAnswer . runCups 100

------------ PART B ------------
partB :: Input -> OutputB
partB input =
  let finalOrder = runCups 10_000_000 . (++ [10 .. 1_000_000]) $ input
      star1 = finalOrder Vec.! 1
      star2 = finalOrder Vec.! star1
   in star1 * star2
