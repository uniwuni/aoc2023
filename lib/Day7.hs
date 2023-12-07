-- |
{-# LANGUAGE OverloadedStrings #-}
module Day7 where
import Data.Text (Text)
import qualified Data.Text as T
-- import Control.Parallel.Strategies
import Data.List (elemIndex, sort, sortOn, group)

parse :: Text -> [(Hand, Int)]
parse t = f <$> l
  where l = T.words <$> T.lines t
        f [x,y] = ((read $ T.unpack x) :: Hand, (read $ T.unpack y) :: Int)
        f _ = error "parsing failure"

parse' :: Text -> [(JokerHand, Int)]
parse' t = f <$> l
  where l = T.words <$> T.lines t
        f [x,y] = ((read $ T.unpack x) :: JokerHand, (read $ T.unpack y) :: Int)
        f _ = error "parsing failure"

newtype Card = Card {cardStrength :: Int} deriving (Eq, Ord)

newtype JokerCard = JokerCard {jokerCardStrength :: Int} deriving (Eq, Ord)

unjoker :: JokerCard -> Card
unjoker = read . show

instance Read Card where
  readsPrec _ x = case elemIndex x cards of
             Just n -> [(Card n, "")]
             Nothing -> error "not a valid card!"
    where cards = pure <$> "23456789TJQKA"

instance Show Card where
  show x = pure $ "23456789TJQKA" !! cardStrength x

instance Read JokerCard where
  readsPrec _ x = case elemIndex x cards of
             Just n -> [(JokerCard n, "")]
             Nothing -> error "not a valid card!"
    where cards = pure <$> "J23456789TQKA"

instance Show JokerCard where
  show x = pure $ "J23456789TQKA" !! jokerCardStrength x



data Hand = Hand Card Card Card Card Card deriving (Eq)
data JokerHand = JokerHand JokerCard JokerCard JokerCard JokerCard JokerCard deriving (Eq)

unjokerHand :: JokerHand -> Hand
unjokerHand (JokerHand c1 c2 c3 c4 c5) = Hand (unjoker c1) (unjoker c2) (unjoker c3) (unjoker c4) (unjoker c5)

instance Read Hand where
  readsPrec _ [x1,x2,x3,x4,x5] =
    [(Hand (read $ pure x1) (read $ pure x2) (read $ pure x3) (read $ pure x4)
      (read $ pure x5), "")]
  readsPrec _ _ = error "not a valid hand!"

instance Read JokerHand where
  readsPrec _ [x1,x2,x3,x4,x5] =
    [(JokerHand (read $ pure x1) (read $ pure x2) (read $ pure x3) (read $ pure x4)
      (read $ pure x5), "")]
  readsPrec _ _ = error "not a valid hand!"


instance Show Hand where
  show (Hand c1 c2 c3 c4 c5) = show c1 ++ show c2 ++ show c3 ++ show c4 ++ show c5

instance Show JokerHand where
  show (JokerHand c1 c2 c3 c4 c5) = show c1 ++ show c2 ++ show c3 ++ show c4 ++ show c5


data HandType = High | Pair | TwoPair | Three | FullHouse | Four | Five deriving (Eq, Ord, Show)
getCounts :: Ord a => [a] -> [(a, Int)]
getCounts l = sortOn snd $ map (\x -> (head x, length x)) $ group $ sort l

handList :: Hand -> [Card]
handList (Hand c1 c2 c3 c4 c5) = [c1,c2,c3,c4,c5]

jokerHandList :: JokerHand -> [JokerCard]
jokerHandList (JokerHand c1 c2 c3 c4 c5) = [c1,c2,c3,c4,c5]

replaceJokerCard :: JokerCard -> JokerCard -> Card
replaceJokerCard x y = if x == read "J" then unjoker y else unjoker x

replaceJoker :: JokerHand -> JokerCard -> Hand
replaceJoker (JokerHand c1 c2 c3 c4 c5) c =
  Hand (replaceJokerCard c1 c) (replaceJokerCard c2 c) (replaceJokerCard c3 c)
    (replaceJokerCard c4 c) (replaceJokerCard c5 c)

allJokerCards :: [JokerCard]
allJokerCards = read . pure <$> "J23456789TQKA"

getType :: Hand -> HandType
getType h
  | length s == 1 = Five
  | length s == 2 && snd (head s) == 1 = Four
  | length s == 2 && snd (head s) == 2 = FullHouse
  | length s == 3 && snd (last s) == 3 = Three
  | length s == 3 && snd (last s) == 2 && snd (s !! 1) == 2 = TwoPair
  | length s == 4 = Pair
  | otherwise = High
  where s = getCounts $ handList h

instance Ord Hand where
  h1 <= h2 = getType h1 < getType h2 || (getType h1 == getType h2 && handList h1 <= handList h2)

getJokerType :: JokerHand -> HandType
getJokerType h = maximum $ getType . replaceJoker h <$> allJokerCards

instance Ord JokerHand where
  h1 <= h2 = getJokerType h1 < getJokerType h2 || (getJokerType h1 == getJokerType h2 && jokerHandList h1 <= jokerHandList h2)

day7part1 :: Text -> String
day7part1 = show . sum . zipWith (*) [1..] . map snd . sortOn fst . parse
day7part2 :: Text -> String
day7part2 = show . sum . zipWith (*) [1..] . map snd . sortOn fst . parse'
