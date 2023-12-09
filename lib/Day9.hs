-- |

module Day9 where

import Data.Text (Text)
import qualified Data.Text as T

parse :: Text -> [[Int]]
parse t = map (read . T.unpack) . T.words <$> T.lines t

delta :: [Int] -> [Int]
delta xs = zipWith (-) (tail xs) xs

deltas :: [Int] -> [[Int]]
deltas = takeWhile (any (/= 0)) . iterate delta

getNext :: [Int] -> Int
getNext = sum . map last . deltas

getNext' :: [Int] -> Int
getNext' = foldr ((-) . head) 0 . deltas

day9part1 :: Text -> String
day9part1 t = show $ sum $ getNext <$> parse t

day9part2 :: Text -> String
day9part2 t = show $ sum $ getNext' <$> parse t
