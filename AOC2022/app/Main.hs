module Main (main)
where

import System.Environment

import Day01
import Day02
import Day03
import Day04
import Day05

main :: IO ()
main = do
  args <- getArgs
  dayPicker args

dayPicker :: [String] -> IO ()
dayPicker [] = putStrLn "Usage: script [day]"
dayPicker ("01":_) = day01
dayPicker ("02":_) = day02
dayPicker ("03":_) = day03
dayPicker ("04":_) = day04
dayPicker ("05":_) = day05

dayPicker _ = putStrLn "Unavailable date"
