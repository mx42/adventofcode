module Main (main) where

import System.Environment

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20


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
dayPicker ("06":_) = day06
dayPicker ("07":_) = day07
dayPicker ("08":_) = day08
dayPicker ("09":_) = day09
dayPicker ("10":_) = day10
dayPicker ("11":_) = day11
dayPicker ("12":_) = day12
dayPicker ("13":_) = day13
dayPicker ("14":_) = day14
dayPicker ("15":_) = day15
dayPicker ("16":_) = day16
dayPicker ("17":_) = day17
dayPicker ("18":_) = day18
dayPicker ("19":_) = day19
dayPicker ("20":_) = day20
dayPicker _ = putStrLn "Unavailable date"
