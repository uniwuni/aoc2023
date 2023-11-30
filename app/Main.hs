{-# LANGUAGE TemplateHaskell #-}

module Main where
import System.Environment

import Day1 (day1)
import Day2 (day2)
import THDays (days)

main :: IO ()
main = do
  [day_str, file] <- getArgs
  let day = (read day_str :: Int) - 1
  contents <- readFile file
  putStrLn $ ($(days 1) !! day) contents
