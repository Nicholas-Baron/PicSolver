{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List (transpose)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (traceShow)
import Row (Row (MkRow), RowConstraint, expandConstraint, filterByKnown, unrow)
import Util

main :: IO ()
main = do
  let possibleRowCounts = product $ map length expandedRows
      possibleColCounts = product $ map length expandedCols

  putStrLn "Total Row Combinations"
  print $ map length expandedRows
  print possibleRowCounts

  putStrLn "Total Column Combinations"
  print $ map length expandedCols
  print possibleColCounts

  let commonRowItems = commonItems expandedRows
      commonColItems = transpose $ commonItems expandedCols

  putStrLn "Common Elements in Rows"
  printKnowledge commonRowItems

  putStrLn "Common Elements in Cols"
  printKnowledge commonColItems

  let knownMatrix = matrixUnion commonColItems commonRowItems
      boardImprovements = iterateWhileDiff improveBoardKnowledge knownMatrix

  putStrLn "Improving Board knowledge"
  mapM_
    ( \board -> do
        printKnowledge board
        putStrLn ""
    )
    boardImprovements

type BoardKnowledge = [[Maybe Bool]]

data Hole = Hole

commonItems :: [Set Row] -> BoardKnowledge
commonItems expandedItems = zipWith genCommonItems expandedItems commonElemMask
  where
    genCommonItems :: Set Row -> [Bool] -> [Maybe Bool]
    genCommonItems set mask = let (MkRow input : _) = Set.elems set in takeFromList mask input

    commonElemMask :: [[Bool]]
    commonElemMask = map (commonElements . map unrow . Set.elems) expandedItems

improveBoardKnowledge :: BoardKnowledge -> BoardKnowledge
improveBoardKnowledge rowKnowledge =
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

expandedRows :: [Set Row]
expandedRows = map (`expandConstraint` exampleBoardSize) rowConstraints

expandedCols :: [Set Row]
expandedCols = map (`expandConstraint` exampleBoardSize) columnConstraints

exampleBoardSize :: Int
exampleBoardSize = 10

columnConstraints, rowConstraints :: [RowConstraint]
rowConstraints = [[5], [2, 2, 1], [2, 3, 1], [4, 3, 1], [1, 1, 1, 1], [3, 1], [5], [1, 2, 1], [1, 1, 1], [7, 1]]
columnConstraints = [[2], [4, 1], [2, 2, 4], [1, 2, 1, 1], [1, 1, 1, 2, 1], [4, 5], [1, 1, 2, 1], [3, 1], [], [5, 3]]
