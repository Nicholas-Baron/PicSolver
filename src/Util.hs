{-# LANGUAGE ScopedTypeVariables #-}

module Util
  ( commonElementMask,
    takeFromList,
    matrixUnion,
    iterateWhileDiff,
  )
where

import Control.Applicative ((<|>))
import Data.List (foldl')

commonElementMask :: forall a. Eq a => [[a]] -> [Bool]
commonElementMask [] = []
commonElementMask (x : xs) = foldl' compareElems initialValue xs
  where
    initialValue = replicate (length x) True

    compareElems :: [Bool] -> [a] -> [Bool]
    compareElems flags = zipWith3 (\flag orig new -> flag && (orig == new)) flags x

takeFromList :: [Bool] -> [a] -> [Maybe a]
takeFromList = zipWith (\t val -> if t then Just val else Nothing)

matrixUnion :: [[Maybe a]] -> [[Maybe a]] -> [[Maybe a]]
matrixUnion = zipWith (zipWith (<|>))

iterateWhileDiff :: Eq a => (a -> a) -> a -> [a]
iterateWhileDiff func input =
  let val = func input
   in if val == input then [val] else val : iterateWhileDiff func val
