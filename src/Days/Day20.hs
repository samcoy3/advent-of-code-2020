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
import Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void

import Data.Functor (($>))
import Control.Applicative.Combinators ((<|>))
import Debug.Trace (traceShowId)
{- ORMOLU_ENABLE -}

runDay :: R.Day
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

type OutputA = Int

type OutputB = [Int]

------------ PART A ------------
-- Given a jigsaw piece, returns the four edges of it as a list
getEdges :: JigsawPiece -> [Edge]
getEdges j =
  let (minX, maxX, minY, maxY) = mapBoundingBox j
   in Edge . Map.elems . (\p -> Map.filterWithKey p j)
        <$> [ (\(x, _) _ -> x == minX),
              (\(x, _) _ -> x == maxX),
              (\(_, y) _ -> y == minY),
              (\(_, y) _ -> y == maxY)
            ]

cornerPieces :: Input -> [Int]
cornerPieces input =
  let -- We create a map from tile ID to their edges
      edgeMap = fmap getEdges input
      -- We create a frequency map of the edges
      edgeFrequency = U.freq . concat . Map.elems $ edgeMap
   in -- We pick the pieces such that two of their corners only appear once in the frequency map
      -- These are exactly the pieces that have two edges not adjacent to another edge
      Map.keys . Map.filter ((== 2) . length) $ fmap (filter (\e -> edgeFrequency Map.! e == 1)) $ edgeMap

partA :: Input -> OutputA
partA = product . cornerPieces

------------ PART B ------------
arrangePieces :: Int -> Input -> [[Int]]
arrangePieces seed input = arrangePieces' [[seed]] (delete seed $ Map.keys input)
  where
    edgeFreqs = U.freq . concat . Map.elems $ fmap getEdges input
    sharesEdgeWith p q = not . null $ getEdges (input Map.! p) `intersect` getEdges (input Map.! q)
    arrangePieces' current@(row : otherRows) remainingPieces =
      let previousRow = if null otherRows then Nothing else Just (head otherRows)
          pieceBelow = fmap reverse previousRow >>= (!!? (length row))
          pieceToRight = if null row then Nothing else Just (head row)
          fittingPieces = filter (\p -> all (\q -> sharesEdgeWith p q) $ catMaybes [pieceBelow, pieceToRight]) remainingPieces
          selectedPiece =
            if isNothing pieceBelow
              then
                head $
                  filter
                    (\p -> any (\e -> edgeFreqs Map.! e /= 2) (getEdges (input Map.! p)))
                    fittingPieces
              else head fittingPieces
       in if
              | null remainingPieces -> current
              | null fittingPieces -> arrangePieces' ([] : row : otherRows) remainingPieces
              | isJust previousRow && ((length $ fromJust previousRow) == length row) ->
                arrangePieces' ([] : row : otherRows) remainingPieces
              | isNothing previousRow && selectedPiece `elem` cornerPieces input ->
                arrangePieces' ([] : (selectedPiece : row) : otherRows) (delete selectedPiece remainingPieces)
              | otherwise -> arrangePieces' ((selectedPiece : row) : otherRows) (delete selectedPiece remainingPieces)

-- Flips a tile vertically.
flipTile :: Map (Int, Int) a -> Map (Int, Int) a
flipTile m =
  let (_, _, minY, maxY) = mapBoundingBox m
   in Map.mapKeys (\(x, y) -> (x, minY + maxY - y)) m

-- Rotates a tile horizontally. Assumes the tile's smallest coordinate is (0,0), and maintains that invariant.
rotateTile :: Map (Int, Int) a -> Map (Int, Int) a
rotateTile m =
  let (_, maxX, _, _) = mapBoundingBox m
   in Map.mapKeys (\(x, y) -> (y, maxX - x)) m

data Side = N | E | S | W

getSide :: Map (Int, Int) Bool -> Side -> [Bool]
getSide m s =
  let (minX, maxX, minY, maxY) = mapBoundingBox m
   in Map.elems . (\p -> Map.filterWithKey p m) $ case s of
        W -> (\(x, _) _ -> x == minX)
        E -> (\(x, _) _ -> x == maxX)
        N -> (\(_, y) _ -> y == minY)
        S -> (\(_, y) _ -> y == maxY)

allTransformations :: Map (Int, Int) a -> [Map (Int, Int) a]
allTransformations m =
  ($ m)
    <$> ( (.)
            <$> [id, flipTile]
            <*> [id, rotateTile, rotateTile . rotateTile, rotateTile . rotateTile . rotateTile]
        )

knitImage :: Input -> [[Int]] -> Map (Int, Int) Bool
knitImage input configuration = removeSurround . stitchRowsTogether . fmap stitchRow $ configuration
  where
    removeSurround m =
      let (minX, maxX, minY, maxY) = mapBoundingBox m
       in Map.filterWithKey
            ( \(x, y) _ ->
                x /= minX && x /= maxX && y /= minY && y /= maxY
            )
            m

    stitchRowsTogether :: [Map (Int, Int) Bool] -> Map (Int, Int) Bool
    stitchRowsTogether = stitchRowsTogether' Map.empty
    stitchRowsTogether' ms rows@(r1 : r2 : rs) =
      if Map.null ms
        then
          let (t1', _) =
                head $
                  [ (t1, t2)
                    | t1 <- [r1, flipTile r1],
                      t2 <- [r2, flipTile r2],
                      getSide t1 S == getSide t2 N
                  ]
           in stitchRowsTogether' t1' rows
        else
          let t =
                head $
                  [ t'
                    | t' <- [r2, flipTile r2],
                      getSide ms S == getSide t' N
                  ]
              (_, _, _, maxYOfmS) = mapBoundingBox ms
           in stitchRowsTogether'
                ( Map.union
                    (Map.filterWithKey (\(_, y) _ -> y /= maxYOfmS) ms)
                    (Map.mapKeys (\(x, y) -> (x, y + maxYOfmS - 1)) t)
                )
                (r2 : rs)
    stitchRowsTogether' ms _ = ms

    stitchRow :: [Int] -> Map (Int, Int) Bool
    stitchRow = stitchRow' Map.empty
    stitchRow' :: Map (Int, Int) Bool -> [Int] -> Map (Int, Int) Bool
    stitchRow' m row@(r1 : r2 : rs) =
      if Map.null m
        then
          let (t1', _) =
                head $
                  [ (t1, t2)
                    | t1 <- allTransformations (input Map.! r1),
                      t2 <- allTransformations (input Map.! r2),
                      getSide t1 E == getSide t2 W
                  ]
           in stitchRow' t1' row
        else
          let t =
                head $
                  [ t'
                    | t' <- allTransformations (input Map.! r2),
                      getSide m E == getSide t' W
                  ]
              (_, maxXOfM, _, _) = mapBoundingBox m
           in stitchRow'
                ( Map.union
                    (Map.filterWithKey (\(x, _) _ -> x /= maxXOfM) m)
                    (Map.mapKeys (\(x, y) -> (x + maxXOfM - 1, y)) t)
                )
                (r2 : rs)
    stitchRow' m _ = m

findSeaMonsters :: Map (Int, Int) Bool -> Set (Int, Int)
findSeaMonsters m =
  let seaMonster = Map.fromList (zip [(0, 18), (1, 0), (1, 5), (1, 6), (1, 11), (1, 12), (1, 17), (1, 18), (1, 19), (2, 1), (2, 4), (2, 7), (2, 10), (2, 13), (2, 16)] (repeat ()))
      findSeaMonsters' m' = Set.unions $ (isSeaMonster m') <$> (Map.keys m')
      isSeaMonster m' (x, y) =
        let seaMonsterMap = Map.mapKeys (\(x', y') -> (x + x', y + y')) seaMonster
         in if Map.isSubmapOfBy (\_ image -> image) seaMonsterMap m'
              then Set.fromList . Map.keys $ seaMonsterMap
              else Set.empty
   in Set.unions $ fmap findSeaMonsters' (allTransformations m)

-- partB :: Input -> OutputB
partB input =
  (\image -> (length . Map.keys . Map.filter id $ image) - (Set.size . findSeaMonsters $ image))
    . knitImage input
    $ arrangePieces (head . cornerPieces $ input) input
