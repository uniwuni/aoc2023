-- |
{-# LANGUAGE OverloadedStrings #-}
module Day6 where
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (transpose)
import Data.Maybe (mapMaybe, fromMaybe)

parse :: Text -> [(Int, Int)]
parse t = mapMaybe toPairs $ transpose l
  where l@[_,_] = tail . T.words <$> T.lines t
        toPairs :: [Text] -> Maybe (Int, Int)
        toPairs [x,y] = Just (read $ T.unpack x, read $ T.unpack y)
        toPairs _ = Nothing

parse' :: Text -> (Int,Int)
parse' t = (read $ T.unpack x, read $ T.unpack y)
  where [[_,x],[_,y]] = T.splitOn ":" . T.replace " " "" <$>  T.lines t

data QuadraticEquation a = QuadraticEquation { quadratic :: a, linear :: a, constant :: a }
  deriving (Eq)

instance Show a => Show (QuadraticEquation a) where
  show x = show (quadratic x) ++ " x^2 + " ++ show (linear x) ++ " x + " ++ show (constant x) ++ " = 0"

data Solution a = None | One a | Two a a deriving Show

solveEquation :: (Floating a, Ord a) => QuadraticEquation a -> Solution a
solveEquation (QuadraticEquation a b c) | b*b - 4*a*c < 0 = None
                                        | abs (b*b - 4*a*c) < 1e-14 = One (-b / (2 * a))
                                        | otherwise = Two ((-b + root)/(2 * a)) ((-b - root)/(2 * a))
                                          where root = sqrt (b*b - 4 * a * c)

-- k * (t - k) - (d+1) = 0
-- -k^2 + t k - (d+1) = 0
-- k: Time pressed, t: time to beat
convert :: (Int, Int) -> QuadraticEquation Double
convert (t,d) = QuadraticEquation (-1) (fromIntegral t) (fromIntegral (-d - 1))

boundaries :: (Int, Int) -> Maybe (Int, Int)
boundaries t = case l of
  None -> Nothing
  One x -> if fromIntegral (round x :: Int) == x then Just (round x,round x) else Nothing
  Two x y -> Just (ceiling (x + 1e-16), floor (y - 1e-16))
  where l = solveEquation $ convert t

day6part1 :: Text -> String
day6part1 t = show $ product $ map (\(x,y) -> abs(y - x) + 1) $ mapMaybe boundaries $ parse t

day6part2 :: Text -> String
day6part2 t = show $ abs (r-l) + 1
  where (x,y) = parse' t
        (l,r) = fromMaybe (0,0) $ boundaries (x,y)
