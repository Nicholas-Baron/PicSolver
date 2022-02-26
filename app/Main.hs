module Main where

import Board
import Debug.Trace (traceShow)

main :: IO ()
main = do
  putStrLn "hello world"

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
         in traceShow input_columns $
              all (uncurry matchesConstraint) $ zip input_columns exampleColConstraints
    )
    exampleBoards
