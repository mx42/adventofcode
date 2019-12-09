module DayPicker
  ( dayPicker
  )
where

import System.Environment

import Day1
import Day2
import Day3
import Day4
import Day5

-- TODO Better way?
load :: [String] -> IO ()
load [] = putStrLn "Usage: script [day]"
load ("1":_) = day1
load ("2":_) = day2
load ("3":_) = day3
load ("4":_) = day4
load ("5":_) = day5
load _ = putStrLn "Unavailable date"

dayPicker :: IO ()
dayPicker = do
  args <- getArgs
  load args
