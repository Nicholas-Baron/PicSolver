module Main where

import Control.Applicative ((<|>))
import Data.List (transpose)
import Row
import Util (commonElements)

main :: IO ()
main = do
  let expandedRows = map (`expandConstraint` exampleBoardSize) rowConstraints
      expandedCols = map (`expandConstraint` exampleBoardSize) columnConstraints

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
  mapM_ print commonRowItems

  putStrLn "Common Elements in Cols"
  mapM_ print commonColItems

  putStrLn "Total Known After 1 step"
  mapM_ print $ elementUnion commonColItems commonRowItems

takeFromList :: [Bool] -> [a] -> [Maybe a]
takeFromList = zipWith (\t val -> if t then Just val else Nothing)

elementUnion :: [[Maybe a]] -> [[Maybe a]] -> [[Maybe a]]
elementUnion = zipWith (zipWith (<|>))

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
