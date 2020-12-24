module Days.Day24 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text

import Data.Functor
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = tile `sepBy` endOfLine
  where
    tile =
      many1 $
        HexVector
          <$> choice
            [ char 'e' $> (1, 0),
              char 'w' $> (-1, 0),
              string "se" $> (1, -1),
              string "sw" $> (0, -1),
              string "nw" $> (-1, 1),
              string "ne" $> (0, 1)
            ]

------------ TYPES ------------
-- We define our Hexagonal Vector datatype
newtype HexVector = HexVector (Int, Int)
  deriving (Eq, Ord)

instance Show HexVector where
  show (HexVector (x, y)) = "[" ++ (show x) ++ ", " ++ (show y) ++ "]"

-- We define a Monoid instance here so that we can just mconcat a list of vectors
instance Semigroup HexVector where
  (HexVector (a, b)) <> (HexVector (c, d)) = HexVector (a + c, b + d)

instance Monoid HexVector where
  mempty = HexVector (0, 0)

type Input = [[HexVector]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Our tile set which is the result of our input directions
initialBlackTiles :: Input -> Set HexVector
initialBlackTiles =
  foldr
    ( \instructions tileSet ->
        let tile = mconcat instructions
         in if tile `Set.member` tileSet
              then Set.delete tile tileSet
              else Set.insert tile tileSet
    )
    Set.empty

partA :: Input -> OutputA
partA = Set.size . initialBlackTiles

------------ PART B ------------
-- A function to return a HexVector's neighbours
neighbours :: HexVector -> [HexVector]
neighbours h =
  fmap
    ((h <>) . HexVector)
    [ (1, 0),
      (0, 1),
      (-1, 0),
      (0, -1),
      (1, -1),
      (-1, 1)
    ]

iterateTiles :: Set HexVector -> Set HexVector
iterateTiles blackTiles =
  let -- This is a map of often tiles appear as a neighbour of an existing black tile
      neighbourFrequencyMap = U.freq . concatMap neighbours $ Set.toList blackTiles
      -- The set of tiles which *could* still be valid black tiles (if they're currently black)
      validOldBlackTiles = Set.fromList . Map.keys $ Map.filter (`elem` [1, 2]) neighbourFrequencyMap
      -- The set of tiles which *could* be valid new black tiles (if they aren't existing black tiles)
      newBlackTiles = Set.fromList . Map.keys $ Map.filter (== 2) neighbourFrequencyMap
   in -- We explicitly check the conditions above, and union the results
      Set.union
        (Set.intersection validOldBlackTiles blackTiles)
        (Set.difference newBlackTiles blackTiles)

partB :: Input -> OutputB
partB = Set.size . (!! 100) . iterate iterateTiles . initialBlackTiles
