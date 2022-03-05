{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Board where

import qualified Data.BitVector as BV
import Data.List (scanl', transpose)

data Block
  = Off
  | On
  deriving (Show, Eq, Enum, Bounded)

fromRow :: Row -> [Block]
fromRow (MkRow row) = map (\block -> if block then On else Off) $ BV.toBits row

toRow :: [Block] -> Row
toRow = MkRow . BV.fromBits . map (== On)

-- A row is a list of blocks and whether they are on, off, or unknown

newtype Row = MkRow BV.BV
  deriving (Show, Eq)

unrow :: Row -> BV.BV
unrow (MkRow bv) = bv

columns :: [Row] -> [Row]
columns =
  map ({-# SCC buildRows #-} MkRow . BV.join)
    . transpose
    . map ({-# SCC unrows #-} BV.group 1 . unrow)

toConstraint :: Row -> RowConstraint
toConstraint (MkRow row) =
  reduceList $
    scanl'
      ( \ !constraint block ->
          {-# SCC block_to_constraint_item #-}
          if block
            then constraint + 1
            else 0
      )
      0
      $ BV.toBits row
  where
    reduceList :: (Num a, Ord a) => [a] -> [a]
    reduceList [] = []
    reduceList [!x] = [x | x /= 0]
    reduceList (x : rest@(y : _)) = if x <= y then {-# SCC reduceList_true #-} reduceList rest else {-# SCC reduceList_false #-} x : reduceList rest

-- A row matches its constraint if:
--    1. it's constraint is <= the minRowLength of the constraint
--    2. none of its constraint blocks are longer than the longest in the constraint
--    3. for each block in the row, the block in the row is <= a block in the same relative order in the constraint
matchesConstraint :: Row -> RowConstraint -> Bool
matchesConstraint row constraint = row `elem` expandConstraint constraint (BV.size $ unrow row)

-- A row constraint is a list of run length encoded "on" blocks
type RowConstraint = [Int]

minRowLength :: RowConstraint -> Int
minRowLength row = sum row + gaps
  where
    gaps = length row - 1

-- Expands the constraint to cover all possible rows of the given length
expandConstraint :: RowConstraint -> Int -> [Row]
expandConstraint [] !row_length = {-# SCC row_cap #-} map MkRow [BV.zeros row_length]
expandConstraint constraint@(block : rest) !row_length
  | minRowLength constraint > row_length = []
  | null rest && block <= row_length =
    {-# SCC last_block #-}
    map
      (\index -> MkRow $ BV.zeros index BV.# BV.ones block BV.# BV.zeros (row_length - (index + block)))
      [0 .. (row_length - block)]
  | otherwise =
    {-# SCC recurse_rows #-}
    [ MkRow (BV.ones block BV.# BV.zeroExtend 1 row)
      | MkRow row <- expandConstraint rest (row_length - (block + 1))
    ]
      ++ [MkRow (BV.zeroExtend 1 row) | MkRow row <- expandConstraint constraint (row_length - 1)]

-- A board is a collection of rows

data Board = Board
  { rows :: BV.BV,
    size :: Int
  }
  deriving (Show, Eq)

fromRows :: [Row] -> Board
fromRows in_rows =
  let boardSize = maximum $ map (BV.size . unrow) in_rows
   in if all (\(MkRow row) -> BV.size row == boardSize) in_rows
        && length in_rows == boardSize
        then Board {rows = BV.join $ map unrow in_rows, size = boardSize}
        else error "Tried to create a non-square board"

toRows :: Board -> [Row]
toRows Board {rows = board_rows, size = board_size} = map MkRow $ BV.group board_size board_rows

-- The column-oriented view of the board
toColumns :: Board -> [Row]
toColumns = columns . toRows

satisfiesConstraints :: Board -> [RowConstraint] -> Bool
satisfiesConstraints board = and . zipWith matchesConstraint (toRows board)

possibleSolutions2 :: [RowConstraint] -> [RowConstraint] -> Int -> [Board]
possibleSolutions2 row_constraints col_constraints board_size =
  map fromRows $
    filter (\(board_rows :: [Row]) -> and $ zipWith matchesConstraint (columns board_rows) col_constraints) $
      mapM (`expandConstraint` board_size) row_constraints
