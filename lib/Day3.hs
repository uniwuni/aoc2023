-- | Day 1
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Day3 where
import Data.Char (isNumber)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Text (Text)
import Data.List (elemIndex)
import Control.Monad
import qualified Data.Vector as V
import Data.Vector.Split as S

vecLines :: Text -> V.Vector (V.Vector Char)
vecLines = V.fromList . map V.fromList . map T.unpack . T.lines

getNumberIndices :: V.Vector (V.Vector Char) -> V.Vector (V.Vector (Int, Int))
getNumberIndices x = indexBlocks
  where indicesIndexed :: V.Vector (V.Vector (Int, Int))
        indicesIndexed = V.map (V.indexed . V.findIndices isNumber) x
        groupedIndices = map (snd <$>) <$> V.groupBy (\(i,a) (j,b) -> j - i == b - a) <$> indicesIndexed
        indexBlocks = V.fromList <$> map (\x -> (V.head x, V.last x)) <$> groupedIndices

-- third is integer value
getNumbers :: V.Vector (V.Vector Char) -> V.Vector (V.Vector (Int, Int, Int))
getNumbers v = V.zipWith zipper indexBlocks v
  where indexBlocks :: V.Vector (V.Vector (Int, Int))
        indexBlocks = getNumberIndices v
        f :: V.Vector Char -> (Int, Int) -> (Int, Int, Int)
        f v (l,r) = (l,r,read $ V.toList slice)
          where slice = V.slice l (r-l+1) v
        zipper :: V.Vector (Int, Int) -> V.Vector Char -> V.Vector (Int, Int, Int)
        zipper x a = V.map (f a) x

getNumbers' :: V.Vector (V.Vector Char) -> V.Vector (Int, (V.Vector (Int, Int, Int)))
getNumbers' = V.indexed . getNumbers

-- line, start, finish, value
getNumbers'' :: V.Vector (V.Vector Char) -> V.Vector (Int, Int, Int, Int)
getNumbers'' = f . getNumbers'
  where f :: V.Vector (Int, (V.Vector (Int, Int, Int))) -> V.Vector (Int, Int, Int, Int)
        f v = v >>= (\(n, w) -> (\(x,y,z) -> (n,x,y,z)) <$> w)

getValue :: V.Vector (Int, Int, Int, Int) -> (Int, Int) -> Int
getValue v (line_asterisk, n_asterisk) = case V.toList filtered of
  [(_,_,_,v1), (_,_,_,v2)] -> v1 * v2
  _ -> 0

  where f :: (Int, Int, Int, Int) -> Bool
        f (line, start, finish, value) = (line_asterisk, n_asterisk) `elem` getSurroundingCoords line (start, finish)
        filtered = V.filter f v


getSurroundingCoords :: Int -> (Int, Int) -> V.Vector (Int, Int)
getSurroundingCoords line (l,r) = V.map (line-1,) lr <> V.fromList [(line,l - 1), (line, r + 1)] <> V.map (line+1,) lr
  where lr = V.enumFromN (l-1) (r - l + 3)

(!?!?) :: V.Vector (V.Vector a) -> (Int, Int) -> Maybe a
vs !?!? (i,j) = do
  v <- vs V.!? i
  v V.!? j

checkSurroundingCoord :: V.Vector (V.Vector Char) -> Int -> (Int, Int, Int) -> Int
checkSurroundingCoord v line (l,r,n) = if condition then n else 0
  where coords = getSurroundingCoords line (l,r)
        values = V.mapMaybe (v !?!?) coords
        condition = any (`elem` ("#$%&*+-/=@" :: String)) values

checkCoords :: V.Vector (V.Vector Char) -> V.Vector (Int, (V.Vector (Int, Int, Int))) -> Int
checkCoords v = sum . V.map (\(line, ps) -> sum $ V.map (checkSurroundingCoord v line) ps)

getStars ::  V.Vector (V.Vector Char) -> V.Vector (Int, Int)
getStars v = join $ V.imap (\n w -> (n,) <$> V.elemIndices '*' w) v

day3part1 :: Text -> String
day3part1 t = show $ checkCoords parsed $ getNumbers' parsed
  where parsed = vecLines t

day3part2 :: Text -> String
day3part2 t = show $ sum $ V.map (getValue numbers) $ getStars parsed
  where parsed = vecLines t
        numbers = getNumbers'' parsed
