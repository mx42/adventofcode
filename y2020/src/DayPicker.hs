module DayPicker
  ( dayPicker
  )
where

import System.Environment

import Day01

-- TODO Better way?
load :: [String] -> IO ()
load [] = putStrLn "Usage: script [day]"
load ("01":_) = day01
load _ = putStrLn "Unavailable date"

dayPicker :: IO ()
dayPicker = do
  args <- getArgs
  load args
