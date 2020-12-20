module Days.Day20 (runDay) where

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

import Data.Functor (($>))
import Control.Applicative.Combinators ((<|>))
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Map.fromList <$> many1 jigsawPiece
  where
    mapper '#' = Just True
    mapper '.' = Just False
    mapper _ = Nothing
    jigsawPiece = do
      string "Tile "
      index <- decimal
      char ':'
      endOfLine
      piece <- coordinateParser mapper 0
      return (index, piece)

coordinateParser :: (Char -> Maybe a) -> Int -> Parser (Map (Int, Int) a)
coordinateParser mapper start = coordinateParser' start start
  where
    coordinateParser' x y =
      choice
        [ (endOfLine >> endOfLine <|> endOfInput) $> Map.empty,
          endOfLine >> coordinateParser' start (y + 1),
          anyChar >>= (\c -> addToMap mapper x y c <$> coordinateParser' (x + 1) y)
        ]
    addToMap mapper x y c = Map.alter (const (mapper c)) (x, y)

------------ TYPES ------------
-- We define a type for our Jigsaw Piece: it's just a map of booleans
type JigsawPiece = Map (Int, Int) Bool

-- We use this newtype because edge equality is a little weird: remember edges can be flipped
newtype Edge = Edge {getEdge :: [Bool]}
  deriving (Show)

instance Eq Edge where
  (Edge e) == (Edge f) = e == f || e == (reverse f)

instance Ord Edge where
  (<=) e f = score e <= score f

-- We use this function so that Ord behaves how Data.Map expects it to
score :: Edge -> Int
score (Edge e') = min (score' e') (score' (reverse e'))
  where
    score' e = sum $ zipWith (\b p -> fromEnum b * p) e (fmap (2 ^) [0 ..])

type Input = Map Int JigsawPiece

type OutputA = [Int]

type OutputB = Void

------------ PART A ------------
-- Given a jigsaw piece, returns the four edges of it as a list
-- Note, we have a magic number here: all tiles in the examples and input have dimensions 10x10
getEdges :: JigsawPiece -> [Edge]
getEdges j =
  Edge . Map.elems . (\p -> Map.filterWithKey p j)
    <$> [ (\(x, _) _ -> x == 0),
          (\(x, _) _ -> x == 9),
          (\(_, y) _ -> y == 0),
          (\(_, y) _ -> y == 9)
        ]

-- partA :: Input -> OutputA
partA input =
  let -- We create a map from tile ID to their edges
      edgeMap = fmap getEdges input
      -- We create a frequency map of the edges
      edgeFrequency = U.freq . concat . Map.elems $ edgeMap
      -- We pick the pieces such that two of their corners only appear once in the frequency map
      -- These are exactly the pieces that have two edges not adjacent to another edge
      cornerPieces = Map.keys . Map.filter ((== 2) . length) $ fmap (filter (\e -> edgeFrequency Map.! e == 1)) $ edgeMap
   in product cornerPieces

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
