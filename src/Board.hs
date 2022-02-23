module Board where

import Data.List (foldl', transpose)

data Block
  = On
  | Off
  | Unknown
  deriving (Show, Eq)

-- A row is a list of blocks and whether they are on, off, or unknown

type Row = [Block]

toConstraint :: Row -> RowConstraint
toConstraint = go []
  where
    go :: [Integer] -> [Block] -> RowConstraint
    -- The constraint is built in reverse order
    go constraint [] = reverse $ filter (/= 0) constraint
    -- Starting a constraint
    go [] (block : blocks) = go [1 | block == On] blocks
    go constraint@(x : rest) (block : blocks) = case block of
      On -> go (x + 1 : rest) blocks
      _ -> go (0 : constraint) blocks

-- A row matches its constraint if:
--    1. it's constraint is <= the minRowLength of the constraint
--    2. none of its constraint blocks are longer than the longest in the constraint
--    3. for each block in the row, the block in the row is <= a block in the same relative order in the constraint
matchesConstraint :: Row -> RowConstraint -> Bool
matchesConstraint row constraint =
  (minRowLength rowConstraint <= minRowLength constraint)
    && (maximum rowConstraint <= maximum constraint)
    && relativeOrderEq rowConstraint constraint
  where
    rowConstraint = toConstraint row

    relativeOrderEq :: RowConstraint -> RowConstraint -> Bool
    relativeOrderEq lhs rhs =
      null $
        foldl'
          (\(currentBlock : restRow) block -> if currentBlock <= block then restRow else currentBlock : restRow)
          lhs
          rhs

-- A row constraint is a list of run length encoded "on" blocks
type RowConstraint = [Integer]

minRowLength :: RowConstraint -> Integer
minRowLength row = sum row + gaps
  where
    gaps :: Integer
    gaps = toInteger $ length row - 1

-- A board is a collection of rows

data Board = Board
  { rows :: [Row],
    size :: Integer
  }

-- The column-oriented view of the board
columns :: Board -> [Row]
columns = transpose . rows
