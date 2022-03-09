{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module BoardKnowledge
  ( BoardKnowledge,
    PossibleBoards (..),
    commonItems,
    improveBoardKnowledge,
    printKnowledge,
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

type BoardKnowledge = [[Maybe Bool]]

fromPossibles :: PossibleBoards -> BoardKnowledge
fromPossibles PossibleBoards {expandedCols, expandedRows} = matrixUnion columnKnowledge rowKnowledge
  where
    columnKnowledge = transpose $ commonItems expandedCols
    rowKnowledge = commonItems expandedRows

commonItems :: [Set Row] -> BoardKnowledge
commonItems expandedItems = zipWith genCommonItems expandedItems commonElemMask
  where
    genCommonItems :: Set Row -> [Bool] -> [Maybe Bool]
    genCommonItems set mask = let (MkRow input : _) = Set.elems set in takeFromList mask input

    commonElemMask :: [[Bool]]
    commonElemMask = map (commonElementMask . map unrow . Set.elems) expandedItems

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
    else matrixUnion commonColItems commonRowItems
  where
    columnKnowledge = transpose rowKnowledge :: BoardKnowledge

    knowledgeFilter debugStr known col =
      let result = filterByKnown known col
       in if Set.null result then (debugStr, showKnowledgeRow known, col, result) `traceShow` result else result

    viableRows = zipWith (knowledgeFilter "row") rowKnowledge expandedRows :: [Set Row]
    viableCols = zipWith (knowledgeFilter "col") columnKnowledge expandedCols :: [Set Row]

    commonRowItems = commonItems viableRows
    commonColItems = transpose $ commonItems viableCols

showKnowledgeRow :: [Maybe Bool] -> String
showKnowledgeRow row =
  '[' :
  map
    ( \case
        Nothing -> 'N'
        Just False -> 'F'
        Just True -> 'T'
    )
    row
    ++ "]"

printKnowledgeRow :: [Maybe Bool] -> IO ()
printKnowledgeRow = putStrLn . showKnowledgeRow

printKnowledge :: BoardKnowledge -> IO ()
printKnowledge = mapM_ printKnowledgeRow
