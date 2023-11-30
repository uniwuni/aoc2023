{-# LANGUAGE TemplateHaskell #-}

module Main where
import System.Environment

import Day1
import Day2
import THDays (days)

main :: IO ()
main = do
  [day_str, part, file] <- getArgs
  let day = (read day_str :: Int) - 1
  let part = (read part :: Int) - 1
  contents <- readFile file
  putStrLn $ ($(days 1) !! (2 * day + part)) contents
