module Days.Day15 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text

import qualified Data.Vector.Unboxed.Mutable as MVec

import Control.Monad.State
import Control.Monad.ST
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` char ','

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- An implementation of the memory game, in the ST monad, with unboxed vectors
-- This runs fast, and is probably the only way to run this fast in Haskell
playGame :: [Int] -> Int -> Int
playGame startingNumbers target = runST $ do
  -- Allocates all of our memory up front
  prevOccurenceVec <- MVec.replicate target (0 :: Int)
  -- Pre-load our vector with the information from our input
  sequence_ $
    fmap (uncurry $ MVec.write prevOccurenceVec) (zip startingNumbers [1 ..])
  let rounds = [length startingNumbers + 1 .. target]
  -- Performs one round of the memory game
  -- Should be blazingly fast, consisting of just one lookup and one write
  let performRound prevVal index = do
        prevOccurence <- MVec.read prevOccurenceVec prevVal
        ( return $ case prevOccurence of
            0 -> 0
            e -> (index - e - 1)
          )
          <* MVec.write prevOccurenceVec prevVal (index - 1)
  -- We perform a monadic fold over all the rounds; this should return the final value written
  foldM performRound (last startingNumbers) rounds

partA :: Input -> OutputA
partA input = playGame input 2020

------------ PART B ------------
partB :: Input -> OutputB
partB input = playGame input 30_000_000
