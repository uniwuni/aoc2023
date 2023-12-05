-- |
{-# LANGUAGE OverloadedStrings #-}
module Day5 where
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (mapMaybe)
import Control.Parallel.Strategies
import Data.Bifunctor
import Data.List
data Line = Line {destination :: Int, source :: Int, len :: Int} deriving (Show, Eq)

parse :: Text -> ([Int], [[Line]])
parse t = (seeds, map (mapMaybe f) hs')
  where ([h]:hs) = T.lines <$> T.splitOn "\n\n" t
        seeds :: [Int]
        seeds = read . T.unpack <$> tail (T.words h)
        hs' = map (map ((read :: String -> Int) . T.unpack)) <$> map T.words
            <$> filter (not . (':' `T.elem`)) <$> hs
        f [x,y,z] = Just $ Line x y z
        f _ = Nothing

parse' :: Text -> ([(Int, Int)], [[Line]])
parse' t = (groups initials, map (map flipLine) ls)
  where (initials, ls) = parse t
        groups [] = []
        groups [_] = []
        groups (x:y:xs) = (x,y) : groups xs

{-# INLINE evalLine #-}
evalLine :: Line -> Int -> Maybe Int
evalLine l n | n >= source l && n < source l + len l = Just $ n - source l + destination l
             | otherwise = Nothing

flipLine :: Line -> Line
flipLine (Line d s l) = Line s d l

evalLines :: [Line] -> Int -> Int
evalLines = foldl' folder id
  where folder f l n = case evalLine l n of
                         Just k -> k
                         Nothing -> f n


evalGame :: [[Line]] -> Int -> Int
evalGame = foldl' (\f g -> evalLines g . f) id

evalGameReverse :: [[Line]] -> Int -> Int
evalGameReverse = foldl' (\f g -> f . evalLines g) id

day5part1 :: Text -> String
day5part1 t = show $ minimum $ evalGame lines' <$> initials
  where (initials, lines') = parse t

isInPairs :: [(Int,Int)] -> Int -> Bool
isInPairs ps n = any (\(a,b) -> n >= a && n < a + b) ps

day5part2 :: Text -> String
day5part2 t = show $ fst $ minimumBy (\x y -> compare (fst x) (fst y))
  ((filter (isInPairs initials . snd)
     (second (evalGameReverse lines') <$> zippedSelf 1 (4*10^(7::Int)))) `using` rdeepseq)
  where (initials, lines') = parse' t
        zippedSelf :: Int -> Int -> [(Int,Int)]
        zippedSelf n k = zip [n..k] [n..k]
