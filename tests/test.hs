{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Row
import Test.Tasty
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC
import Util (commonElements)

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
          ( map
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
          ( map
              MkRow
              [ [True, True, True, True, False],
                [False, True, True, True, True]
              ]
          )
          (expandConstraint [4] 5),
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
             in QC.conjoin [predicate row | predicate <- predicates, row <- expandedConstraints]
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
        ( commonElements [[1, 2, 3], [3, 2, 1], [2, 2, 2]] HU.@?= [False, True, False]
        )
    ]
