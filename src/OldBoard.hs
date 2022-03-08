{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OldBoard where

import Control.Parallel.Strategies (parMap, rdeepseq, rpar)
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
  deriving (Eq)

instance Show Row where
  show (MkRow row) = BV.showBin row

unrow :: Row -> BV.BV
unrow (MkRow bv) = bv

columns :: [Row] -> [Row]
columns =
  parMap rpar ({-# SCC buildRows #-} MkRow . BV.join)
    . transpose
    . parMap rpar ({-# SCC unrows #-} BV.group (1 :: Int) . unrow)

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
matchesConstraint row constraint = toConstraint row == constraint

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
    let addBlank = BV.zeroExtend (1 :: Int)
        rows_with_block = parMap rpar (MkRow . BV.append (BV.ones block) . addBlank . unrow) $ expandConstraint rest (row_length - (block + 1))
        rows_without_block = [MkRow (addBlank row) | MkRow row <- expandConstraint constraint (row_length - 1)]
     in rows_with_block ++ rows_without_block

-- A board is a collection of rows

data Board = Board
  { rows :: BV.BV,
    size :: Int
  }
  deriving (Show, Eq, Ord)

fromRows :: [Row] -> Board
fromRows in_rows =
  let raw_rows = parMap rpar unrow in_rows
      boardSize = maximum $ parMap rdeepseq BV.size raw_rows
   in if all (\row -> BV.size row == boardSize) raw_rows
        && length raw_rows == boardSize
        then Board {rows = BV.join raw_rows, size = boardSize}
        else error ("Tried to create a non-square board with dimensions " ++ show (map BV.size raw_rows))

toRows :: Board -> [Row]
toRows Board {rows = board_rows, size = board_size} = map MkRow $ BV.group board_size board_rows

-- The column-oriented view of the board
toColumns :: Board -> [Row]
toColumns = columns . toRows

boardFlip :: Board -> Board
boardFlip = fromRows . toColumns

satisfiesConstraints :: Board -> [RowConstraint] -> Bool
satisfiesConstraints board = and . zipWith matchesConstraint (toRows board)

possibleSolutions2 :: [RowConstraint] -> [RowConstraint] -> Int -> [Board]
possibleSolutions2 row_constraints col_constraints board_size =
  map fromRows $
    filter (\(board_rows :: [Row]) -> and $ zipWith matchesConstraint (columns board_rows) col_constraints) $
      mapM (`expandConstraint` board_size) row_constraints