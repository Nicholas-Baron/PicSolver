{-# LANGUAGE LambdaCase #-}

module Row
  ( RowConstraint,
    expandConstraint,
    Row (..),
    unrow,
    filterByKnown,
    minRowLength,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set

newtype Row = MkRow [Bool]
  deriving (Eq, Ord)

unrow :: Row -> [Bool]
unrow (MkRow row) = row

instance Show Row where
  show (MkRow row) = '[' : map (\val -> if val then 'T' else 'F') row ++ "]"

-- Expands the constraint to cover all possible rows of the given length
expandConstraint :: RowConstraint -> Int -> Set Row
expandConstraint [] row_length = Set.fromList $ map MkRow [replicate row_length False]
expandConstraint constraint@(block : rest) row_length
  | minRowLength constraint > row_length = Set.empty
  | null rest && block <= row_length =
    Set.fromList $
      map
        (\index -> MkRow $ replicate index False ++ addBlock (replicate (row_length_after_block - index) False))
        [0 .. (row_length - block)]
  | otherwise =
    let addBlank = (:) False
        rows_with_block = Set.map (MkRow . addBlock . addBlank . unrow) $ expandConstraint rest (row_length_after_block - 1)
        rows_without_block = Set.fromList [MkRow (addBlank row) | MkRow row <- Set.toList $ expandConstraint constraint (row_length - 1)]
     in rows_with_block `Set.union` rows_without_block
  where
    addBlock row = replicate block True ++ row
    row_length_after_block = row_length - block

filterByKnown :: [Maybe Bool] -> Set Row -> Set Row
filterByKnown knowns = Set.filter go
  where
    go :: Row -> Bool
    go (MkRow row) =
      all
        ( \case
            (_, Nothing) -> True
            (actual, Just expected) -> actual == expected
        )
        $ zip row knowns

type RowConstraint = [Int]

minRowLength :: RowConstraint -> Int
minRowLength [] = 0
minRowLength row = sum row + length row - 1
