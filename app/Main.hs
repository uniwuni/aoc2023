{-# LANGUAGE TemplateHaskell #-}

module Main where
import System.Environment
import System.TimeIt
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Day1
import Day2
import THDays (days)

main :: IO ()
main = do
  [day_str, part_str, file] <- getArgs
  let day = (read day_str :: Int) - 1
  let part = (read part_str :: Int) - 1
  contents <- T.readFile file
  timeIt $ putStrLn $ ($(days 1) !! (2 * day + part)) contents
