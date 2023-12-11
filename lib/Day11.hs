-- |
{-# LANGUAGE TupleSections #-}
module Day11 where
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V
import Data.List (transpose, elemIndices)

parse :: Text -> [[Bool]]
parse t = ((== '#') <$>) . T.unpack <$> T.lines t

addLines, addColumns :: [[Bool]] -> [[Bool]]
addLines = foldr f []
  where f x y | all not x = x:x:y
              | otherwise = x:y

addColumns = transpose . addLines . transpose

getLines, getColumns :: [[Bool]] -> [Int]
getLines = foldr f []
  where f x y | all not x = 1000000:y
              | otherwise = 1:y

getColumns = getLines . transpose

findGalaxies :: [[Bool]] -> [(Int, Int)]
findGalaxies t = concat $ zipWith g [0..] l
  where l = elemIndices True <$> t
        g n xs = (n,) <$> xs

pairGalaxies :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
pairGalaxies xs = [(a,b) | a <- xs, b <- xs, a < b]

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1,y1) (x2,y2) = abs (x2 - x1) + abs (y2 - y1)

distance' :: V.Vector Int -> V.Vector Int -> (Int, Int) -> (Int, Int) -> Int
distance' ls cs (x1,y1) (x2,y2) = V.sum (V.slice (min x1 x2) (abs (x2 - x1)) ls) + V.sum (V.slice (min y1 y2) (abs (y2 - y1)) cs)

day11part1 :: Text -> String
day11part1 = show . sum . map (uncurry distance) . pairGalaxies .  findGalaxies . addLines . addColumns . parse

day11part2 :: Text -> String
day11part2 t = show $ sum $ map (uncurry $ distance' ls cs) $ pairGalaxies $ findGalaxies p
  where p = parse t
        cs = V.fromList $ getColumns p
        ls = V.fromList $ getLines p
