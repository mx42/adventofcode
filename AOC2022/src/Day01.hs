module Day01 (day01) where

import Data.List.Split (splitOn)
import Data.List (sort)

parseInput :: String -> [Int]
parseInput = reverse . sort . map (sum . map read . lines) . splitOn "\n\n"

day01 :: IO ()
day01 = do
  putStrLn "AoC 2022 day 1"
  input <- getContents
  putStrLn ""
  let weights = parseInput input
  putStrLn "Part1:"
  print (sum $ take 1 weights)
  putStrLn "Part2:"
  print (sum $ take 3 weights)
