module Days.Day15 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List as L
import qualified Data.Map.Strict as Map

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text

import Control.Monad.State
import Data.STRef
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
-- A lazy, infinite list approach - this is good enough for Part A
infiniteGameList :: [Int] -> [Int]
infiniteGameList startingNumbers =
  let initials = init startingNumbers
      startingMap = foldl' (\m (p, v) -> Map.insert v p m) Map.empty (zip [0 ..] initials)
   in (initials ++) $
        unfoldr
          ( \(currentEntry, map, index) ->
              let nextEntry = case map Map.!? currentEntry of
                    Nothing -> 0
                    Just e -> index - e
                  newMap = Map.insert currentEntry index map
               in Just (currentEntry, (nextEntry, newMap, index + 1))
          )
          (last startingNumbers, startingMap, length initials)

partA :: Input -> OutputA
partA input = (infiniteGameList input) !! (2020 - 1)

------------ PART B ------------
-- A version of the above, basically, but in the ST monad.
-- This takes 5 (!) minutes, so I plan to rework it soon, possibly using the `hashtables` package
partB :: Input -> OutputB
partB input = runST $ do
  map <- newSTRef (foldl' (\map (pos, val) -> Map.insert val pos map) Map.empty (zip [1 ..] $ init input))
  let rounds = [length input + 1 .. 30_000_000]
  (\roundAction -> foldM roundAction (last input) rounds) $
    ( \prevVal index -> do
        currentMap <- readSTRef map
        ( return $ case currentMap Map.!? prevVal of
            Nothing -> 0
            Just e -> (index - e - 1)
          )
          <* modifySTRef' map (Map.insert prevVal (index - 1))
    )
