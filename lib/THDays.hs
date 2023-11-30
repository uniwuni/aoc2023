-- | Template haskell machinery for days

module THDays (days) where
import Language.Haskell.TH

days :: Int -> ExpQ
days n = do
  let names = mkName <$> ("day"++) <$> show <$> [1..n]
  listE $ varE <$> names
