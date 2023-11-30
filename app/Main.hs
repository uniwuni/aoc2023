module Main where
import System.Environment
import Day
main :: IO ()
main = do
  [day_str, file] <- getArgs
  let day = (read day_str :: Int)
  contents <- readFile file
  return ()
