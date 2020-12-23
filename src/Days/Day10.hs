module Days.Day10 (runDay, combinationsOfDiffList) where

{- ORMOLU_DISABLE -}
import Data.List

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text

import Util.Util
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` endOfLine

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Sorts the input list and counts the differences of length 1 and 3.
-- We add one to the total differences of length 3, because the question specifies this.
partA :: Input -> OutputA
partA input =
  let diffList =
        (\ls -> zipWith (flip (-)) (0 : ls) ls)
          . sort
          $ input
   in ((+ 1) . length . filter (== 3) $ diffList) * (length . filter (== 1) $ diffList)

------------ PART B ------------
{-
Counts the number of ways in which a list consisting of 1s and 2s can be "grouped", into a list containing 1s, 2s, and 3s.
The function works recursively in the following way:
 - Either we "add" the first two values together, or we don't.
 - If we don't, then the number of arrangements is equal to the number of arrangements in the list with the first element removed.
 - (Note that we can only add the first two terms together if their sum is <= 3.)
 - If we do add the first two together, then the number of ways of arranging the list is the number of arrangements of the list from the third element onwards, with the sum of the first pair prepended.
 - Finally, note that these two cases (adding the first pair, not adding them) are mutually exclusive; they must result in different groupings. Therefore we add them, rather than multiply them.
-}
combinationsOfDiffList :: [Int] -> Int
combinationsOfDiffList ls = case ls of
  [] -> 1
  _ : [] -> 1
  l : l' : ls' ->
    combinationsOfDiffList (l' : ls')
      + ( if l + l' <= 3
            then combinationsOfDiffList ((l + l') : ls')
            else 0
        )

{-
We construct our difference list as before.
This time, however, we group it into sublists of elements less than three.
Why? Well, we're counting the number of possible selections of the adapters, and when the difference is three we are forced to pick both adapters on each side of this difference anyway.
On the sublists composed of elements less than three, we count the number of ways of "partially combining" adjacent pairs of this list so that all elements in the list remain at most three.
The number of ways of performing such "partial combinations" is the same as the number of ways of omitting adapters such that all adapters have differences of less than three from each other (proof is left to the reader).
Finally, since our choice of which adapters we skip in each of these "sublists" is independent of each other, we multiply together the number of combinations in each sublist to get the total number of adapter selections.
-}
partB :: Input -> OutputB
partB input =
  let diffList =
        chunksByPredicate (/= 3)
          . (\ls -> zipWith (flip (-)) (0 : ls) ls)
          . sort
          $ input
   in product $ fmap combinationsOfDiffList diffList
