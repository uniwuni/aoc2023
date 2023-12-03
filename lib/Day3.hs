-- | Day 1
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Day3 where
import Data.Char (isNumber)
import qualified Data.Text as T
import Data.Text (Text)
import Data.List (elemIndex)
import Control.Monad
import qualified Data.Vector as V
import Data.Vector.Split as S

uncurry3 f (x,y,z) = f x y z

vecLines :: Text -> V.Vector (V.Vector Char)
vecLines = V.fromList . map (V.fromList . T.unpack) . T.lines

getNumberIndices :: V.Vector (V.Vector Char) -> V.Vector (V.Vector (Int, Int))
getNumberIndices x = indexBlocks
  where indicesIndexed :: V.Vector (V.Vector (Int, Int))
        indicesIndexed = V.map (V.indexed . V.findIndices isNumber) x
        groupedIndices = map (snd <$>) . V.groupBy (\(i,a) (j,b) -> j - i == b - a) <$> indicesIndexed
        indexBlocks = V.fromList . map (\x -> (V.head x, V.last x)) <$> groupedIndices

data NumberBlock = NB { start' :: Int, finish' :: Int, value' :: Int } deriving (Eq, Read, Show)

-- third is integer value
getNumbers :: V.Vector (V.Vector Char) -> V.Vector (V.Vector NumberBlock)
getNumbers v = V.zipWith zipper indexBlocks v
  where indexBlocks :: V.Vector (V.Vector (Int, Int))
        indexBlocks = getNumberIndices v
        f :: V.Vector Char -> (Int, Int) -> NumberBlock
        f v (l,r) = NB l r $ read $ V.toList slice
          where slice = V.slice l (r-l+1) v

        zipper :: V.Vector (Int, Int) -> V.Vector Char -> V.Vector NumberBlock
        zipper x a = V.map (f a) x

getNumbers' :: V.Vector (V.Vector Char) -> V.Vector (Int, V.Vector NumberBlock)
getNumbers' = V.indexed . getNumbers

data FullNumberBlock = FNB {line :: Int, start :: Int, finish :: Int, value :: Int} deriving (Eq, Show, Read)

-- line, start, finish, value
getNumbers'' :: V.Vector (V.Vector Char) -> V.Vector FullNumberBlock
getNumbers'' = f . getNumbers'
  where f :: V.Vector (Int, V.Vector NumberBlock) -> V.Vector FullNumberBlock
        f v = v >>= (\(n, w) -> (\(NB s f v) -> FNB n s f v) <$> w)

getValue :: V.Vector FullNumberBlock -> (Int, Int) -> Int
getValue v (ln, n_asterisk) = case V.toList filtered of
  [(_,val), (_,val')] -> val * val'
  _ -> 0
  where filtered = V.filter (((ln, n_asterisk) `elem`) . fst) v2
        v2 = V.map (\x -> (getSurroundingCoords (line x) (start x, finish x), value x)) $
               V.filter (\x -> line x >= ln - 1 && line x <= ln + 1) v


getSurroundingCoords :: Int -> (Int, Int) -> V.Vector (Int, Int)
getSurroundingCoords line (l,r) = V.map (line-1,) lr <> V.fromList [(line,l - 1), (line, r + 1)] <> V.map (line+1,) lr
  where lr = V.enumFromN (l-1) (r - l + 3)

(!?!?) :: V.Vector (V.Vector a) -> (Int, Int) -> Maybe a
vs !?!? (i,j) = do
  v <- vs V.!? i
  v V.!? j

checkSurroundingCoord :: V.Vector (V.Vector Char) -> Int -> NumberBlock -> Int
checkSurroundingCoord v line block = if condition then value' block else 0
  where coords = getSurroundingCoords line (start' block, finish' block)
        values = V.mapMaybe (v !?!?) coords
        condition = any (`elem` ("#$%&*+-/=@" :: String)) values

checkCoords :: V.Vector (V.Vector Char) -> V.Vector (Int, V.Vector NumberBlock) -> Int
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
