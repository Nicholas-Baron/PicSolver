{-# LANGUAGE InstanceSigs #-}

import Board
import Test.Tasty
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC

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
          (toConstraint $ MkRow [On, Off, On, On, On, Off, On])
          (toConstraint $ MkRow [On, Off, Unknown, On, On, On, Off, Off, On]),
      QC.testProperty
        "at least 1 On"
        (\(MkRow contents) -> On `elem` contents),
      QC.testProperty
        "matchesConstraint"
        (\row@(MkRow contents) -> matchesConstraint row (toConstraint row)),
      QC.testProperty
        "minRowLength"
        (\row@(MkRow contents) -> minRowLength (toConstraint row) <= toInteger (length contents))
    ]

instance QC.Arbitrary Row where
  arbitrary :: QC.Gen Row
  arbitrary = MkRow <$> QC.suchThat arbitraryRow (elem On)
    where
      arbitraryRow :: QC.Gen [Block]
      arbitraryRow = QC.listOf1 arbitraryBlock

      arbitraryBlock :: QC.Gen Block
      arbitraryBlock = QC.elements [On, Off, Unknown]
