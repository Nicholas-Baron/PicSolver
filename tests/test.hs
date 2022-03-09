{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Set as Set
import Row
import Test.Tasty
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC
import Util

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [utilTests, rowTests]

rowTests :: TestTree
rowTests = testGroup "Row Tests" [expandConstraintProperties]

expandConstraintProperties :: TestTree
expandConstraintProperties =
  testGroup
    "expandConstraint"
    [ HU.testCase "expandConstraint with blocks of 1" $
        HU.assertEqual
          "expandConstraint handles blocks of size 1"
          ( Set.fromList $
              map
                MkRow
                [ [True, False, False, False, False],
                  [False, True, False, False, False],
                  [False, False, True, False, False],
                  [False, False, False, True, False],
                  [False, False, False, False, True]
                ]
          )
          (expandConstraint [1] 5),
      HU.testCase "expandConstraint base case" $
        HU.assertEqual
          "expandConstraint handles blocks of size 4"
          ( Set.fromList $
              map
                MkRow
                [ [True, True, True, True, False],
                  [False, True, True, True, True]
                ]
          )
          (expandConstraint [4] 5),
      HU.testCase "filterByKnown pt. 1" $
        HU.assertEqual
          "filterByKnown will ignore Nothing"
          ( Set.fromList $
              map
                MkRow
                [ [True, True, True, True, False],
                  [False, True, True, True, True]
                ]
          )
          (filterByKnown (replicate 5 Nothing) $ expandConstraint [4] 5),
      HU.testCase "filterByKnown pt. 2" $
        HU.assertEqual
          "filterByKnown will match Justs"
          ( Set.singleton $
              MkRow [True, True, True, True, False]
          )
          (filterByKnown (Just True : replicate 4 Nothing) $ expandConstraint [4] 5),
      QC.testProperty
        "all rows match the constraint they expanded from"
        ( \(MkTestConstraint (constraint, row_length)) ->
            let expandedConstraints = expandConstraint constraint row_length
                predicates :: [Row.Row -> QC.Property]
                predicates =
                  map
                    (\predicate row -> QC.counterexample (show (row, constraint, row_length)) (predicate row))
                    [ \row -> QC.property $ row `matchesConstraint` constraint,
                      \(MkRow row) -> length row QC.=== row_length
                    ]
             in QC.conjoin [predicate row | predicate <- predicates, row <- Set.elems expandedConstraints]
        )
    ]

newtype TestConstraint = MkTestConstraint (RowConstraint, Int)
  deriving (Show)

instance QC.Arbitrary TestConstraint where
  arbitrary :: QC.Gen TestConstraint
  arbitrary = MkTestConstraint <$> QC.suchThat arbitraryTuple (\(constraint, row_length) -> minRowLength constraint <= row_length)
    where
      arbitraryTuple :: QC.Gen (RowConstraint, Int)
      arbitraryTuple = do
        row_length <- QC.suchThat QC.arbitrary (>= 5)
        let numbers = QC.listOf1 QC.arbitrary
        constraint <- map QC.getPositive <$> numbers
        return (constraint, row_length)

utilTests :: TestTree
utilTests =
  testGroup
    "Utility Tests"
    [ HU.testCase "commonElements of [] is []" (commonElements ([] :: [[Int]]) HU.@?= []),
      QC.testProperty
        "commonElements of the same list should all be equal"
        (\(list :: [Int]) -> and $ commonElements $ replicate 5 list),
      HU.testCase
        "commonElements of [[1,2,3], [3,2,1], [2,2,2]] is [False,True,False]"
        (commonElements [[1, 2, 3], [3, 2, 1], [2, 2, 2]] HU.@?= [False, True, False]),
      QC.testProperty
        "takeFromList will have the length of the shorter list"
        ( \(list :: [Int], mask) ->
            let expectedLength = min (length list) (length mask)
             in length (takeFromList mask list) == expectedLength
        ),
      QC.testProperty
        "takeFromList will put Just in all places with True in the mask"
        ( \(list :: [Int], mask) ->
            QC.conjoin $
              zipWith
                ( \b -> \case
                    Nothing -> not b
                    Just _ -> b
                )
                mask
                $ takeFromList mask list
        ),
      QC.testProperty
        "takeFromList will put the original value in the Just"
        ( \(list :: [Int], mask) ->
            QC.conjoin $
              zipWith
                ( \item ->
                    \case
                      Nothing -> True
                      Just x -> x == item
                )
                list
                $ takeFromList mask list
        ),
      QC.testProperty "iterateWhileDiff id only returns 1 item" (\(item :: Int) -> length (iterateWhileDiff id item) == 1),
      HU.testCase "iterateWhileDiff (`div` 2) 100 == [50, 25, 12, 6, 3, 1, 0, 0]" $ iterateWhileDiff (`div` 2) 100 HU.@?= [50, 25, 12, 6, 3, 1, 0, 0]
    ]
