import BoardKnowledge
import Data.List (transpose)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (traceShow)
import Row (Row (MkRow), RowConstraint, expandConstraint, filterByKnown, unrow)
import Util

main :: IO ()
main = do
  let knownMatrix = fromPossibles possibleBoards
      possibleBoards = fromConstraints exampleBoardSize rowConstraints columnConstraints
      boardImprovements = iterateWhileDiff (improveBoardKnowledge possibleBoards) knownMatrix

  let possibleRowCounts = product $ map length $ expandedRows possibleBoards
      possibleColCounts = product $ map length $ expandedCols possibleBoards

  putStrLn "Total Row Combinations"
  print $ map length $ expandedRows possibleBoards
  print possibleRowCounts

  putStrLn "Total Column Combinations"
  print $ map length $ expandedCols possibleBoards
  print possibleColCounts

  putStrLn "Improving Board knowledge"
  mapM_
    ( \board -> do
        printKnowledge board
        putStrLn ""
    )
    boardImprovements

exampleBoardSize :: Int
exampleBoardSize = 10

columnConstraints, rowConstraints :: [RowConstraint]
rowConstraints = [[5], [2, 2, 1], [2, 3, 1], [4, 3, 1], [1, 1, 1, 1], [3, 1], [5], [1, 2, 1], [1, 1, 1], [7, 1]]
columnConstraints = [[2], [4, 1], [2, 2, 4], [1, 2, 1, 1], [1, 1, 1, 2, 1], [4, 5], [1, 1, 2, 1], [3, 1], [], [5, 3]]
