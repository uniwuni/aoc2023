module Day8 where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Map (Map)
import Data.Char (ord)
import Data.Either (fromLeft)
import Control.Monad (foldM)
-- True = Left, False = Right

nameToInt :: Char -> Char -> Char -> Int
nameToInt x y z = ord x * 2^(16 :: Int) + ord y * 2^(8 :: Int) + ord z

isStart, isEnd :: Int -> Bool
isStart = (== ord 'A') . (`mod` 256)
isEnd = (== ord 'Z') . (`mod` 256)

start, goal :: Int
start = nameToInt 'A' 'A' 'A'
goal = nameToInt 'Z' 'Z' 'Z'

parse :: Text -> ([Bool], Map Int (Int, Int))
parse t = (map (== 'L') (T.unpack dirs), M.fromList $ makeTriple <$> rest)
  where dirs:_:rest = T.lines t
        makeTriple l = (nameToInt (T.index l 0) (T.index l 1) (T.index l 2),
                        (nameToInt (T.index l 7) (T.index l 8) (T.index l 9),
                        nameToInt (T.index l 12) (T.index l 13) (T.index l 14)))

follow :: Map Int (Int, Int) -> Bool -> Int -> Int
follow mp dir p = if dir then l else r
  where (l,r) = M.findWithDefault (0,0) p mp

getCount :: (Int -> Bool) -> Map Int (Int, Int) -> [Bool] -> Int -> Int
getCount endP graph dirs' k = fromLeft (-1) l
  where dirs = cycle dirs'
        f (point, n) dir | endP point = Left n
                         | otherwise = Right (follow graph dir point, n+1)
        l :: Either Int (Int, Int)
        l = foldM f (k, 0 :: Int) dirs

day8part1 :: Text -> String
day8part1 t = show $ getCount (== goal) graph dirs' start
  where (dirs', graph) = parse t

day8part2 :: Text -> String
day8part2 t = show $ foldl lcm 1 numbers
  where (dirs', graph) = parse t
        starts = filter isStart $ M.keys graph
        numbers = getCount isEnd graph dirs' <$> starts
