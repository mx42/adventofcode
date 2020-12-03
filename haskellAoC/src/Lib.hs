module Lib (pickYear) where

import Y2015.Days
import Y2019.Days
import Y2020.Days

import Data.List

import System.Directory
import System.Environment

type DayFun = [String] -> (String, String)

getDayFun :: String -> String -> DayFun
getDayFun "2015" = year2015
getDayFun "2019" = year2019
getDayFun "2020" = year2020

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
