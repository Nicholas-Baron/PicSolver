module Main where

import Board

main :: IO ()
main = do
  putStr "Count of example boards: "
  print $ length exampleBoards
  putStr "Count of possible solution boards: "
  print $ length exampleSolutions

exampleRowConstraints :: [RowConstraint]
exampleRowConstraints = [[4], [2, 1], [3], [1], [2]]

exampleBoardSize :: Int
exampleBoardSize = 5

exampleBoards :: [Board]
exampleBoards = possibleSolutions exampleRowConstraints exampleBoardSize

exampleColConstraints :: [RowConstraint]
exampleColConstraints = [[4], [3, 1], [1, 1], [1], [2]]

exampleSolutions :: [Board]
exampleSolutions =
  filter
    ( \board ->
        let input_columns = columns board
         in all (uncurry matchesConstraint) $ zip input_columns exampleColConstraints
    )
    exampleBoards
