module Main where

import Board
import Control.Parallel.Strategies (parMap, rpar)
import qualified Data.Set as Set

main :: IO ()
main = do
  let expandedRows = parMap rpar (`expandConstraint` exampleBoardSize) rowConstraints
  let expandedCols = parMap rpar (`expandConstraint` exampleBoardSize) columnConstraints

  let possibleRows = sequence expandedRows
  let possibleCols = sequence expandedCols

  let possibleRowBoards = map fromRows possibleRows
  let possibleColBoards = map (boardFlip . fromRows) possibleCols

  let possibleRowCounts = product $ map length expandedRows
  let possibleColCounts = product $ map length expandedCols

  putStrLn "Total Row Combinations"
  print possibleRowCounts

  putStrLn "Total Column Combinations"
  print possibleColCounts

  putStrLn "Int Max Bound"
  print (maxBound :: Int)

  let (smallerList, largerSet) =
        if length expandedRows < length expandedCols
          then (possibleRowBoards, Set.fromList possibleColBoards)
          else (possibleColBoards, Set.fromList possibleRowBoards)

  print $ filter (`Set.member` largerSet) smallerList

  print "Done"

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
