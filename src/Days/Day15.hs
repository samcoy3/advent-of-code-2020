module Days.Day15 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List as L
import qualified Data.Map.Strict as Map

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text

import Control.Monad.State
import Control.Monad.ST

import qualified Data.HashTable.ST.Cuckoo as HT
import System.IO.Unsafe
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
-- An implementation of the memory game in the ST monad, using the hashtables package
playGame :: [Int] -> Int -> Int
playGame startingNumbers target = runST $ do
  -- We allocate a hashtable with 5 million buckets. This is roughly enough.
  prevsMap <- HT.newSized 5_000_000
  -- Sequence over the input to initialise our hash table.
  sequence_ $
    fmap (\(p, v) -> HT.insert prevsMap v p) (zip [1 ..] $ init startingNumbers)
  -- Monadically fold over thirty million iterations.
  -- This is, well, slow. It takes about 2 and a half minutes on my machine.
  -- Not sure a better implementation in Haskell is possible.
  let rounds = [length startingNumbers + 1 .. target]
  let performRound prevVal index = do
        prevOccurence <- HT.lookup prevsMap prevVal
        ( return $ case prevOccurence of
            Nothing -> 0
            Just e -> (index - e - 1)
          )
          <* HT.insert prevsMap prevVal (index - 1)
  foldM performRound (last startingNumbers) rounds

partA :: Input -> OutputA
partA input = playGame input 2020

------------ PART B ------------
partB :: Input -> OutputB
partB input = playGame input 30_000_000
