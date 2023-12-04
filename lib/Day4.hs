-- |
{-# LANGUAGE OverloadedStrings #-}
module Day4 where
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (mapMaybe)
import Data.List (intersect, transpose)

zipWithLongest :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipWithLongest f x y xs ys = map head . transpose $   -- longest length;
                [                                 --   view from above:
                  zipWith f xs
                      (ys ++ repeat y)            -- with length of xs
                , zipWith f (xs ++ repeat x)
                       ys                         -- with length of ys
                ]


parse :: Text -> [([Int],[Int])]
parse = mapMaybe (f .  (map (map (read . T.unpack) . T.words) . tail . T.split (`elem` (":|" :: String)))) . T.lines
  where f [x,y] = Just (x,y)
        f _ = Nothing

score :: [Int] -> [Int] -> Int
score xs ys = if l == 0 then 0 else 2^(l-1)
  where l = length $ intersect xs ys

scoreNext :: [Int] -> [Int] -> [Int] -> ([Int], Int)
scoreNext [] xs ys = (replicate l 1, 1)
  where l = length $ intersect xs ys
scoreNext (r:rs) xs ys = (zipWithLongest (+) 0 0 rs (replicate l r), r)
  where l = length $ intersect xs ys

run :: [([Int],[Int])] -> ([Int], Int)
run x = foldl f (replicate (length x) 1, 0) x
  where f :: ([Int], Int) -> ([Int], [Int]) -> ([Int], Int)
        f (state, sum) (l,r) = (p, q + sum)
          where (p,q) = scoreNext state l r

day4part1 :: Text -> String
day4part1 = show . sum . map (uncurry score) . parse

day4part2 :: Text -> String
day4part2 = show . snd . run . parse
