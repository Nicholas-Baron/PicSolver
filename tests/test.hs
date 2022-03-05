{-# LANGUAGE InstanceSigs #-}

import Board
import qualified Data.BitVector as BV
import Test.Tasty
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [boardTests]

boardTests :: TestTree
boardTests = testGroup "Board Tests" [rowTests]

rowTests :: TestTree
rowTests =
  testGroup
    "Row Tests"
    [ HU.testCase "toConstraint" $
        HU.assertEqual
          "toConstraint ignores the number of off or unknown blocks"
          (toConstraint $ toRow [On, Off, On, On, On, Off, On])
          (toConstraint $ toRow [On, Off, Off, On, On, On, Off, Off, On]),
      HU.testCase "expandConstraint with blocks of 1" $
        HU.assertEqual
          "expandConstraint handles blocks of size 1"
          ( map
              toRow
              [ [On, Off, Off, Off, Off],
                [Off, On, Off, Off, Off],
                [Off, Off, On, Off, Off],
                [Off, Off, Off, On, Off],
                [Off, Off, Off, Off, On]
              ]
          )
          (expandConstraint [1] 5),
      HU.testCase "expandConstraint base case" $
        HU.assertEqual
          "expandConstraint handles blocks of size 4"
          ( map
              toRow
              [ [On, On, On, On, Off],
                [Off, On, On, On, On]
              ]
          )
          (expandConstraint [4] 5),
      HU.testCase "columns" $
        let input_rows = map toRow [[On, On, On], [Off, On, Off], [On, Off, Off]]
            board = fromRows input_rows
            output_rows = map toRow [[On, Off, On], [On, On, Off], [On, Off, Off]]
         in HU.assertEqual "columns transposes the board" output_rows (toColumns board),
      QC.testProperty
        "no zeros in toConstraint"
        (\(MkTestRow row) -> notElem 0 $ toConstraint row),
      QC.testProperty
        "at least 1 On"
        (\(MkTestRow (MkRow row)) -> BV.signumI row /= 0),
      QC.testProperty
        "matchesConstraint"
        (\(MkTestRow row) -> matchesConstraint row (toConstraint row)),
      QC.testProperty
        "minRowLength"
        (\(MkTestRow row) -> minRowLength (toConstraint row) <= (BV.size $ unrow row)),
      QC.testProperty
        "expandConstraint"
        ( \(MkTestConstraint (constraint, row_length)) ->
            let expandedConstraints = expandConstraint constraint row_length
                predicates :: [Row -> QC.Property]
                predicates =
                  map
                    (\predicate row -> QC.counterexample (show (row, toConstraint row)) (predicate row))
                    [ \row -> QC.property $ row `matchesConstraint` constraint,
                      \(MkRow row) -> BV.size row QC.=== row_length
                    ]
             in QC.conjoin [predicate row | predicate <- predicates, row <- expandedConstraints]
        )
    ]

newtype TestConstraint = MkTestConstraint (RowConstraint, Int)
  deriving (Show, Eq)

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

newtype TestRow = MkTestRow Row
  deriving (Show, Eq)

instance QC.Arbitrary TestRow where
  arbitrary :: QC.Gen TestRow
  arbitrary = MkTestRow <$> toRow <$> QC.suchThat arbitraryRow (elem On)
    where
      arbitraryRow :: QC.Gen [Block]
      arbitraryRow = QC.listOf1 arbitraryBlock

      arbitraryBlock :: QC.Gen Block
      arbitraryBlock = QC.arbitraryBoundedEnum
