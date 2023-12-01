-- | Day 1
{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import Data.Char (isNumber)
import qualified Data.List.Safe as S
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.List (elemIndex)
import Data.Bifunctor

minIndex, maxIndex :: Ord a => [a] -> Maybe Int
minIndex xs = elemIndex (minimum xs) xs
maxIndex xs = elemIndex (maximum xs) xs

digits :: [T.Text]
digits = ["one","two","three","four","five","six","seven","eight","nine"] ++ (T.pack <$> show <$> [1..9])

parse :: String -> [String]
parse = lines

extractFromLine' :: String -> Maybe Int
extractFromLine' s = do
  h' <- firstIndex
  l' <- lastIndex
  let h = (h' `mod` 9) + 1
  let l = (l' `mod` 9) + 1
  pure $ 10 * h + l
  where
        indices_first = fst <$> flip T.breakOn (T.pack s) <$> digits
        indices_last = fst <$> flip T.breakOnEnd (T.pack s) <$> digits
        firstIndex, lastIndex :: Maybe Int
        (first, firstIndex) = (minimum indices_first, minIndex indices_first)
        (last, lastIndex) = (maximum indices_last, maxIndex indices_last)

extractFromLine :: String -> Maybe Int
extractFromLine xs = do
  h <- S.head ys
  l <- S.last ys
  pure $ 10 * read [h] + read [l]
  where ys = filter isNumber xs

extractTotal :: [String] -> Int
extractTotal = sum . catMaybes . map extractFromLine

extractTotal' :: [String] -> Int
extractTotal' = sum . catMaybes . map extractFromLine'

day1part1 :: String -> String
day1part1 = show . extractTotal . parse

day1part2 :: String -> String
day1part2 = show . extractTotal' . parse
