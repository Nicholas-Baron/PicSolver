module Row
  ( RowConstraint,
    expandConstraint,
    Row (..),
    matchesConstraint,
    minRowLength,
    unrow,
  )
where

import Data.List (scanl')

newtype Row = MkRow [Bool]
  deriving (Eq, Ord)

unrow :: Row -> [Bool]
unrow (MkRow row) = row

instance Show Row where
  show (MkRow row) = '[' : map (\val -> if val then 'T' else 'F') row ++ "]"

-- Expands the constraint to cover all possible rows of the given length
expandConstraint :: RowConstraint -> Int -> [Row]
expandConstraint [] row_length = map MkRow [replicate row_length False]
expandConstraint constraint@(block : rest) row_length
  | minRowLength constraint > row_length = []
  | null rest && block <= row_length =
    map
      (\index -> MkRow $ replicate index False ++ replicate block True ++ replicate (row_length - (index + block)) False)
      [0 .. (row_length - block)]
  | otherwise =
    let addBlank = (:) False
        rows_with_block = map (MkRow . (\row -> replicate block True ++ row) . addBlank . unrow) $ expandConstraint rest (row_length - (block + 1))
        rows_without_block = [MkRow (addBlank row) | MkRow row <- expandConstraint constraint (row_length - 1)]
     in rows_with_block ++ rows_without_block

-- A row matches its constraint if:
--    1. it's constraint is <= the minRowLength of the constraint
--    2. none of its constraint blocks are longer than the longest in the constraint
--    3. for each block in the row, the block in the row is <= a block in the same relative order in the constraint
matchesConstraint :: Row -> RowConstraint -> Bool
matchesConstraint row constraint = minRowLength (toConstraint row) <= minRowLength constraint

toConstraint :: Row -> RowConstraint
toConstraint (MkRow row) =
  reduceList $
    scanl'
      ( \constraint block ->
          if block
            then constraint + 1
            else 0
      )
      0
      row
  where
    reduceList :: (Num a, Ord a) => [a] -> [a]
    reduceList [] = []
    reduceList [x] = [x | x /= 0]
    reduceList (x : rest@(y : _)) = if x <= y then reduceList rest else x : reduceList rest

type RowConstraint = [Int]

minRowLength :: RowConstraint -> Int
minRowLength row = sum row + length row - 1
