-- | Day 2

{-# LANGUAGE OverloadedStrings #-}

module Day2 where
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (catMaybes)

data Pair = Pair {color :: Text, number :: Int} deriving (Eq, Show)
newtype Set = Set {unset :: [Pair]} deriving (Eq, Show)

lookupPair :: Text -> [Pair] -> Maybe Int
lookupPair t xs = lookup t $ (\x -> (color x, number x)) <$> xs

parse :: Text -> [[Set]]
parse t = (map (Set . catMaybes . map parseEntry)) <$> l
  where l = map (T.words <$>) <$> (T.splitOn "," <$>) <$>
              tail <$> T.split (`elem` (";:" :: String)) <$> T.lines t
        parseEntry :: [Text] -> Maybe Pair
        parseEntry [a,b] = Just $ Pair b $ read $ T.unpack a
        parseEntry _ = Nothing

-- | first argument is the true configuration
isPossible :: Set -> Set -> Bool
isPossible (Set x) (Set y) = all f y
  where f (Pair t n) = case lookupPair t x of
                          Nothing -> False
                          Just m  -> m >= n

isPossibleLine :: Set -> [Set] -> Bool
isPossibleLine s = all (isPossible s)

minimumNeeded :: [Set] -> Set
minimumNeeded xs = Set $ foldr folder [] $ concatMap unset xs
  where
    folder :: Pair -> [Pair] -> [Pair]
    folder p@(Pair color number) ps = case lookupPair color ps of
                          Nothing -> p:ps
                          Just m -> if number > m then map f ps else ps
                            where f p'@(Pair color' number') | color == color' = Pair color number
                                                             | otherwise = p'
power :: Set -> Int
power = product . map number . unset

config :: Set
config = Set [Pair "red" 12, Pair "blue" 14, Pair "green" 13]

day2part1 :: T.Text -> String
day2part1 = show . sum . map fst . filter (isPossibleLine config . snd) . zip [1 :: Int ..] . parse


day2part2 :: T.Text -> String
day2part2 = show . sum . map (power . minimumNeeded) . parse
