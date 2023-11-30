-- | Template haskell machinery for days

module THDays (days) where
import Language.Haskell.TH

strings :: Int -> [String]
strings n = ((++"part1") <$> prefixes) ++ ((++"part2") <$> prefixes)
  where prefixes = ("day"++) <$> show <$> [1..n]

days :: Int -> ExpQ
days n = do
  let names = mkName <$> strings n
  listE $ varE <$> names
