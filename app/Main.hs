{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List (transpose)
import Row
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

  let commonRowElems = map (commonElements . map unrow) expandedRows
      commonColElems = map (commonElements . map unrow) expandedCols

  let commonRowItems = zipWith (\(row : _) to_take -> takeFromList to_take (unrow row)) expandedRows commonRowElems
      commonColItems = transpose $ zipWith (\(row : _) to_take -> takeFromList to_take (unrow row)) expandedCols commonColElems

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

improveBoardKnowledge :: BoardKnowledge -> BoardKnowledge
improveBoardKnowledge rowKnowledge = matrixUnion commonColItems commonRowItems
  where
    columnKnowledge = transpose rowKnowledge :: BoardKnowledge

    commonRowElems = map (commonElements . map unrow) $ zipWith filterByKnown rowKnowledge expandedRows :: [[Bool]]
    commonColElems = map (commonElements . map unrow) $ zipWith filterByKnown columnKnowledge expandedCols :: [[Bool]]

    commonRowItems = zipWith (\(row : _) to_take -> takeFromList to_take (unrow row)) expandedRows commonRowElems :: BoardKnowledge
    commonColItems = transpose $ zipWith (\(row : _) to_take -> takeFromList to_take (unrow row)) expandedCols commonColElems :: BoardKnowledge

printKnowledge :: BoardKnowledge -> IO ()
printKnowledge = mapM_ go
  where
    go :: [Maybe Bool] -> IO ()
    go row =
      putStrLn $
        '[' :
        map
          ( \case
              Nothing -> 'N'
              Just False -> 'F'
              Just True -> 'T'
          )
          row
          ++ "]"

expandedRows :: [[Row]]
expandedRows = map (`expandConstraint` exampleBoardSize) rowConstraints

expandedCols :: [[Row]]
expandedCols = map (`expandConstraint` exampleBoardSize) columnConstraints

{-
exampleBoardSize :: Int
exampleBoardSize = 15

rowConstraints = [[2, 2, 4], [2, 1, 1], [4, 1, 2, 1], [1, 1, 1, 2, 1], [2, 1, 1, 2], [1, 1, 2, 1], [2, 1, 2, 2, 3], [3, 2, 1, 3], [1, 2, 1, 1, 2], [2, 1, 2, 2, 1], [1, 1, 2], [2, 3, 1], [1, 2, 1, 2, 1], [2, 1, 1], [1, 2, 1, 1]]

columnConstraints = [[1, 1, 1, 1, 2], [3, 3, 1, 1, 1], [2, 1, 1], [1, 2, 4, 2], [1, 1, 1, 1, 1, 1], [1, 1, 1, 1, 1], [1, 5, 1, 1], [2, 3, 1, 1, 2], [1, 1, 1, 1], [2, 1, 1], [2, 1, 1, 1, 1], [1, 2, 1, 3, 1], [1, 1, 2], [1, 1, 1, 5, 1], [1, 2, 1, 1, 1]]

-}

exampleBoardSize :: Int
exampleBoardSize = 10

rowConstraints :: [RowConstraint]
rowConstraints = [[1, 1, 1, 1], [1, 2, 2], [2, 2, 1], [1, 1, 3, 1], [2, 1], [4, 1, 3], [2, 2], [2], [2, 3], [3]]

columnConstraints :: [RowConstraint]
columnConstraints = [[1, 2], [4, 2, 1], [1, 1], [1, 3, 1], [1, 1, 1, 1], [3, 1, 2], [2, 2, 2], [2, 1, 1], [2, 2], [1, 1, 2]]
