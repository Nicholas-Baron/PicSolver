module Board where

import Data.List (foldl', transpose)

data Block
  = Unknown
  | Off
  | On
  deriving (Show, Eq, Enum, Bounded)

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
matchesConstraint row = relativeOrderEq rowConstraint
  where
    rowConstraint = toConstraint row

    relativeOrderEq :: RowConstraint -> RowConstraint -> Bool
    relativeOrderEq lhs rhs =
      null $
        foldl'
          ( \currentRow block ->
              case currentRow of
                (currentBlock : restRow) -> if currentBlock <= block then restRow else currentBlock : restRow
                [] -> []
          )
          lhs
          rhs

-- A row constraint is a list of run length encoded "on" blocks
type RowConstraint = [Integer]

minRowLength :: RowConstraint -> Integer
minRowLength row = sum row + gaps
  where
    gaps :: Integer
    gaps = toInteger $ length row - 1

-- Expands the constraint to cover all possible rows of the given length
expandConstraint :: RowConstraint -> Int -> [Row]
expandConstraint [] row_length = [replicate row_length Off]
expandConstraint constraint@(block : rest) row_length
  | minRowLength constraint > toInteger row_length = []
  | null rest && block <= toInteger row_length =
    let int_block = fromInteger block
     in map
          (\index -> replicate index Off ++ replicate int_block On ++ replicate (row_length - (index + int_block)) Off)
          [0 .. (row_length - int_block)]
  | otherwise =
    let int_block = fromInteger block
     in [replicate int_block On ++ Off : row | row <- expandConstraint rest (row_length - (int_block + 1))]
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

possibleSolutions :: [RowConstraint] -> Int -> [Board]
possibleSolutions row_constraints board_size =
  map (\board_rows -> Board {size = board_size, rows = board_rows}) $
    mapM (`expandConstraint` board_size) row_constraints
