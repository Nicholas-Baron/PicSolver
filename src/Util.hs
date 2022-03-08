{-# LANGUAGE ScopedTypeVariables #-}

module Util
  ( commonElements,
  )
where

import Data.List (foldl')

commonElements :: forall a. Eq a => [[a]] -> [Bool]
commonElements [] = []
commonElements (x : xs) = foldl' compareElems initialValue xs
  where
    initialValue = replicate (length x) True

    compareElems :: [Bool] -> [a] -> [Bool]
    compareElems flags = zipWith3 (\flag orig new -> flag && (orig == new)) flags x
