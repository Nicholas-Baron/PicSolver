{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import BoardKnowledge
import Row (RowConstraint, minRowLength)
import System.Environment (getArgs)
import System.Exit (die)
import Text.JSON
import Util

main :: IO ()
main = do
  args <- getArgs

  input_file <- if null args then die "Input file not specified" else return (head args)

  input_text <- readFile input_file

  Constraints {boardSize, columnConstraints, rowConstraints} <- case decodeStrict input_text of Ok x -> return x; Error msg -> die msg

  let knownMatrix = fromPossibles possibleBoards
      possibleBoards = fromConstraints boardSize rowConstraints columnConstraints
      boardImprovements = iterateWhileDiff (improveBoardKnowledge possibleBoards) knownMatrix

  let possibleRowCounts = product $ map length $ expandedRows possibleBoards
      possibleColCounts = product $ map length $ expandedCols possibleBoards

  putStrLn "Total Row Combinations"
  print $ map length $ expandedRows possibleBoards
  print possibleRowCounts

  putStrLn "Total Column Combinations"
  print $ map length $ expandedCols possibleBoards
  print possibleColCounts

  putStrLn "Inital Board Knowledge"
  print knownMatrix

  putStrLn "Improving Board knowledge"
  mapM_
    ( \board -> do
        putStrLn $ show (100 * completeness board) ++ "% complete"
        print board
        putStrLn ""
    )
    boardImprovements

  putStr "Solved in "
  putStr $ show (length boardImprovements + 1)
  putStrLn " steps"

data Constraints = Constraints
  { boardSize :: Int,
    columnConstraints :: [RowConstraint],
    rowConstraints :: [RowConstraint]
  }
  deriving (Show)

instance JSON Constraints where
  readJSON (input :: JSValue) = do
    object <- case input of JSObject object -> Ok (fromJSObject object); _ -> Error (show input ++ " is not a JSObject")

    let fallibleLookup name = case lookup name object of
          Nothing -> Error ("Could not find " ++ name ++ " in " ++ show object)
          Just x -> readJSON x
        validate :: (Show a) => String -> (a -> Bool) -> a -> Result a
        validate msg predicate value =
          if predicate value
            then Ok value
            else Error $ msg ++ " : " ++ show value

    boardSize <- fallibleLookup "boardSize" >>= validate "Board size must be positive" (> 0)

    let fitConstraints = all ((<= boardSize) . minRowLength)

    rowConstraints <- fallibleLookup "rowConstraints" >>= validate "Rows must fit in the specified size" fitConstraints
    columnConstraints <- fallibleLookup "columnConstraints" >>= validate "Columns must fit in the specified size" fitConstraints

    return $ Constraints {boardSize, columnConstraints, rowConstraints}

  showJSON Constraints {boardSize, rowConstraints, columnConstraints} =
    JSObject $
      toJSObject
        [ ("boardSize", showJSON boardSize),
          ("rowConstraints", showJSON rowConstraints),
          ("columnConstraints", showJSON columnConstraints)
        ]
