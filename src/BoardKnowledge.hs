{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module BoardKnowledge
  ( BoardKnowledge,
    PossibleBoards (..),
    commonItems,
    improveBoardKnowledge,
    fromConstraints,
    fromPossibles,
  )
where

import Data.List (transpose)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Row
import Util

newtype BoardKnowledge = MkBoardKnowledge [[Maybe Bool]]
  deriving (Eq)

instance Show BoardKnowledge where
  show (MkBoardKnowledge input) = unlines $ map showKnowledgeRow input
    where
      showKnowledgeRow :: [Maybe Bool] -> String
      showKnowledgeRow row = '[' : map (\case Nothing -> 'N'; Just False -> 'F'; Just True -> 'T') row ++ "]"

transposeKnowledge :: BoardKnowledge -> BoardKnowledge
transposeKnowledge (MkBoardKnowledge board) = MkBoardKnowledge (transpose board)

fromUnion :: BoardKnowledge -> BoardKnowledge -> BoardKnowledge
fromUnion (MkBoardKnowledge lhs) (MkBoardKnowledge rhs) = MkBoardKnowledge (matrixUnion lhs rhs)

fromPossibles :: PossibleBoards -> BoardKnowledge
fromPossibles PossibleBoards {expandedCols, expandedRows} = fromUnion columnKnowledge rowKnowledge
  where
    columnKnowledge = transposeKnowledge $ commonItems expandedCols
    rowKnowledge = commonItems expandedRows

commonItems :: [Set Row] -> BoardKnowledge
commonItems expandedItems = MkBoardKnowledge $ zipWith genCommonItems expandedItems commonElemMask
  where
    genCommonItems :: Set Row -> [Bool] -> [Maybe Bool]
    genCommonItems set mask = let (MkRow input : _) = Set.elems set in takeFromList mask input

    commonElemMask :: [[Bool]]
    commonElemMask = map (commonElementMask . map unrow . Set.elems) expandedItems

filterRowSet :: BoardKnowledge -> [Set Row] -> [Set Row]
filterRowSet (MkBoardKnowledge knowledge) = zipWith knowledgeFilter knowledge
  where
    knowledgeFilter :: [Maybe Bool] -> Set Row -> Set Row
    knowledgeFilter known col =
      let result = filterByKnown known col
       in if Set.null result then (show known, col, result) `traceShow` result else result

data PossibleBoards = PossibleBoards
  { expandedCols :: [Set Row],
    expandedRows :: [Set Row]
  }

fromConstraints :: Int -> [RowConstraint] -> [RowConstraint] -> PossibleBoards
fromConstraints boardSize rows cols =
  PossibleBoards
    { expandedCols = map (`expandConstraint` boardSize) cols,
      expandedRows = map (`expandConstraint` boardSize) rows
    }

improveBoardKnowledge :: PossibleBoards -> BoardKnowledge -> BoardKnowledge
improveBoardKnowledge PossibleBoards {expandedCols, expandedRows} rowKnowledge =
  if any Set.null viableRows || any Set.null viableCols
    then error $ "Found empty rows/columns: " ++ show (viableRows, viableCols)
    else fromUnion commonColItems commonRowItems
  where
    columnKnowledge = transposeKnowledge rowKnowledge :: BoardKnowledge

    viableRows = filterRowSet rowKnowledge expandedRows :: [Set Row]
    viableCols = filterRowSet columnKnowledge expandedCols :: [Set Row]

    commonRowItems = commonItems viableRows
    commonColItems = transposeKnowledge $ commonItems viableCols
