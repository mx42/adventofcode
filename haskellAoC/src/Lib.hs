module Lib (pickYear) where

import Y2019.Days
import Y2020.Days

import Data.List

import System.Directory
import System.Environment

type DayFun = [String] -> (String, String)

getDayFun :: String -> String -> DayFun
getDayFun "2019" "01" = y19day01
getDayFun "2019" "02" = y19day02
getDayFun "2019" "03" = y19day03
getDayFun "2019" "04" = y19day04
getDayFun "2019" "05" = y19day05
getDayFun "2019" "06" = y19day06
getDayFun "2019" "07" = y19day07
getDayFun "2019" "08" = y19day08
getDayFun "2019" "09" = y19day09
getDayFun "2019" "10" = y19day10
getDayFun "2019" "11" = y19day11
getDayFun "2019" "12" = y19day12
getDayFun "2019" "13" = y19day13
getDayFun "2020" "01" = y20day01
getDayFun "2020" "02" = y20day02
getDayFun "2020" "03" = y20day03

callDailyFun :: String -> DayFun -> String -> IO ()
callDailyFun year fn name = do
  putStrLn $ "With input " ++ name
  input <- readFile ("inputs/" ++ year ++ "/" ++ name)
  let (part1, part2) = fn $ lines input
  putStrLn $ "Part1: " ++ part1
  putStrLn $ "Part2: " ++ part2

getFiles :: String -> String -> IO [String]
getFiles year day = listDirectory ("inputs/" ++ year ++ "/") >>=
                    return . filter (isPrefixOf day)

dayPicker :: String -> String -> IO ()
dayPicker year day = do
  let dailyFun = getDayFun year day
  files       <- getFiles year day
  _           <- mapM (callDailyFun year dailyFun) files
  putStrLn     $ "Ending process"


load :: [String] -> IO ()
load (year:day:_) = dayPicker year day
load _ = putStrLn "Usage: script [year] [day]"

pickYear :: IO ()
pickYear = do
  args <- getArgs
  load args
