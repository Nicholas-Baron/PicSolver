{-# LANGUAGE ScopedTypeVariables #-}

module Util
  ( commonElements,
    takeFromList,
    matrixUnion,
  )
where

import Control.Applicative ((<|>))
import Data.List (foldl')

commonElements :: forall a. Eq a => [[a]] -> [Bool]
commonElements [] = []
commonElements (x : xs) = foldl' compareElems initialValue xs
  where
    initialValue = replicate (length x) True

    compareElems :: [Bool] -> [a] -> [Bool]
    compareElems flags = zipWith3 (\flag orig new -> flag && (orig == new)) flags x

takeFromList :: [Bool] -> [a] -> [Maybe a]
takeFromList = zipWith (\t val -> if t then Just val else Nothing)

matrixUnion :: [[Maybe a]] -> [[Maybe a]] -> [[Maybe a]]
matrixUnion = zipWith (zipWith (<|>))
