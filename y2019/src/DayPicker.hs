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
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11
import Day12
import Day13
import Day14

-- TODO Better way?
load :: [String] -> IO ()
load [] = putStrLn "Usage: script [day]"
load ("1":_) = day1
load ("2":_) = day2
load ("3":_) = day3
load ("4":_) = day4
load ("5":_) = day5
load ("6":_) = day6
load ("7":_) = day7
load ("8":_) = day8
load ("9":_) = day9
load ("10":_) = day10
load ("11":_) = day11
load ("12":_) = day12
load ("13":_) = day13
load ("14":_) = day14
load _ = putStrLn "Unavailable date"

dayPicker :: IO ()
dayPicker = do
  args <- getArgs
  load args
