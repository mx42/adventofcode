module DayPicker
  ( dayPicker
  )
where

import System.Environment

import Day1
import Day2

load :: [String] -> IO ()
load [] = putStrLn "Usage: script [day]"
load ("1":_) = day1
load ("2":_) = day2
load _ = putStrLn "Unavailable date"

dayPicker :: IO ()
dayPicker = do
  args <- getArgs
  load args
