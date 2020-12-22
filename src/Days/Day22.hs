module Days.Day22 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Foldable (toList)
import Data.Sequence ((|>), Seq (Empty, (:<|)))
import qualified Data.Sequence as Seq (take, length, fromList)
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  string "Player 1:"
  endOfLine
  p1 <- decimal `sepBy` endOfLine
  count 2 endOfLine
  string "Player 2:"
  endOfLine
  p2 <- decimal `sepBy` endOfLine
  return (Seq.fromList p1, Seq.fromList p2)

------------ TYPES ------------
type Input = (Seq Int, Seq Int)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Plays a game of Combat, returns the winner's score
playCombat :: (Seq Int, Seq Int) -> Int
playCombat (Empty, deck) = score deck
playCombat (deck, Empty) = score deck
playCombat ((c1 :<| s1), (c2 :<| s2)) =
  if c1 > c2
    then playCombat (s1 |> c1 |> c2, s2)
    else playCombat (s1, s2 |> c2 |> c1)

-- Scores a winning sequence
score :: Seq Int -> Int
score = sum . zipWith (*) [1 ..] . reverse . toList

partA :: Input -> OutputA
partA = playCombat

------------ PART B ------------
data Winner = Player1 | Player2 deriving (Eq, Show)

-- Plays a game of Recursive Combat
-- We keep our previous deck states in a set, so we can easily check for duplicates
playRecursiveCombat :: Set (Seq Int, Seq Int) -> (Seq Int, Seq Int) -> (Winner, Seq Int)
-- If a deck is empty, the other player wins
playRecursiveCombat _ (Empty, deck) = (Player2, deck)
playRecursiveCombat _ (deck, Empty) = (Player1, deck)
playRecursiveCombat prevs ds@(d1@(c1 :<| s1), (c2 :<| s2)) =
  -- We define the actions we take if player 1 and player 2 win the round, respectively
  -- We make sure to add the current position to the set of previous positions
  -- These are lazily evaluated, so we don't need to worry that the calls will actually happen unless we call them in the "in"
  let player1Win = playRecursiveCombat (Set.insert ds prevs) (s1 |> c1 |> c2, s2)
      player2Win = playRecursiveCombat (Set.insert ds prevs) (s1, s2 |> c2 |> c1)
   in if
          -- If we've seen the position before, player 1 wins
          | ds `Set.member` prevs -> (Player1, d1)
          -- If we have enough cards, we play recursive combat
          -- We make sure to start with no memory of previous positions
          | (Seq.length s1 >= c1) && (Seq.length s2 >= c2) ->
            case playRecursiveCombat Set.empty (Seq.take c1 s1, Seq.take c2 s2) of
              (Player1, _) -> player1Win
              (Player2, _) -> player2Win
          -- Otherwise we just compare the cards, as before
          | c1 > c2 -> player1Win
          | otherwise -> player2Win

partB :: Input -> OutputB
partB = score . snd . playRecursiveCombat Set.empty
