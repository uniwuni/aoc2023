-- | Day 1
{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import Data.Char (isNumber)
import qualified Data.List.Safe as S
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.List (elemIndex)

minIndex, maxIndex :: Ord a => [a] -> Maybe Int
minIndex xs = elemIndex (minimum xs) xs
maxIndex xs = elemIndex (maximum xs) xs

digits :: [T.Text]
digits = ["one","two","three","four","five","six","seven","eight","nine"] ++ (T.pack . show <$> [(1 :: Int)..9])

parse :: T.Text -> [T.Text]
parse = T.lines

extractFromLine' :: T.Text -> Maybe Int
extractFromLine' s = do
  h' <- firstIndex
  l' <- lastIndex
  let h = (h' `mod` 9) + 1
  let l = (l' `mod` 9) + 1
  pure $ 10 * h + l
  where
        indices_first = fst . flip T.breakOn s <$> digits
        indices_last = fst . flip T.breakOnEnd s <$> digits
        firstIndex, lastIndex :: Maybe Int
        (first, firstIndex) = (minimum indices_first, minIndex indices_first)
        (last, lastIndex) = (maximum indices_last, maxIndex indices_last)

extractFromLine :: T.Text -> Maybe Int
extractFromLine xs = do
  (h,_) <- T.uncons ys
  (_,l) <- T.unsnoc ys
  pure $ 10 * read [h] + read [l]
  where ys = T.filter isNumber xs

extractTotal :: [T.Text] -> Int
extractTotal = sum . mapMaybe extractFromLine

extractTotal' :: [T.Text] -> Int
extractTotal' = sum . mapMaybe extractFromLine'

day1part1 :: T.Text -> String
day1part1 = show . extractTotal . parse

day1part2 :: T.Text -> String
day1part2 = show . extractTotal' . parse
