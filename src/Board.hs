{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Board where

import Data.List (scanl', transpose)

data Block
  = Unknown
  | Off
  | On
  deriving (Show, Eq, Enum, Bounded)

-- A row is a list of blocks and whether they are on, off, or unknown

type Row = [Block]

toConstraint :: Row -> RowConstraint
toConstraint =
  reduceList
    . scanl'
      ( \ !constraint block ->
          {-# SCC block_to_constraint_item #-}
          case block of
            On -> constraint + 1
            _ -> 0
      )
      0
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
matchesConstraint row = relativeOrderEq (toConstraint row)
  where
    relativeOrderEq :: RowConstraint -> RowConstraint -> Bool
    relativeOrderEq [] _ = True
    relativeOrderEq _ [] = False
    relativeOrderEq in_row@(block : restRow) (constraint : restConstraint) =
      {-# SCC cond_relativeOrderEq #-}
      relativeOrderEq (if block <= constraint then restRow else in_row) restConstraint

-- A row constraint is a list of run length encoded "on" blocks
type RowConstraint = [Int]

minRowLength :: RowConstraint -> Int
minRowLength row = sum row + gaps
  where
    gaps = length row - 1

-- Expands the constraint to cover all possible rows of the given length
expandConstraint :: RowConstraint -> Int -> [Row]
expandConstraint [] !row_length = [replicate row_length Off]
expandConstraint constraint@(block : rest) !row_length
  | minRowLength constraint > row_length = []
  | null rest && block <= row_length =
    map
      (\index -> replicate index Off ++ replicate block On ++ replicate (row_length - (index + block)) Off)
      [0 .. (row_length - block)]
  | otherwise =
    [replicate block On ++ Off : row | row <- expandConstraint rest (row_length - (block + 1))]
      ++ [Off : row | row <- expandConstraint constraint (row_length - 1)]

-- A board is a collection of rows

data Board = Board
  { rows :: [Row],
    size :: Int
  }
  deriving (Show, Eq)

fromRows :: [Row] -> Board
fromRows in_rows =
  let boardSize = maximum $ map length in_rows
   in if all (\row -> length row == boardSize) in_rows
        && length in_rows == boardSize
        then Board {rows = in_rows, size = boardSize}
        else error "Tried to create a non-square board"

-- The column-oriented view of the board
columns :: Board -> [Row]
columns = transpose . rows

satisfiesConstraints :: Board -> [RowConstraint] -> Bool
satisfiesConstraints Board {rows = boardRows} = and . zipWith matchesConstraint boardRows

possibleSolutions2 :: [RowConstraint] -> [RowConstraint] -> Int -> [Board]
possibleSolutions2 row_constraints col_constraints board_size =
  map (\board_rows -> Board {size = board_size, rows = board_rows}) $
    filter (\board_rows -> and $ zipWith matchesConstraint (transpose board_rows) col_constraints) $
      mapM (`expandConstraint` board_size) row_constraints
