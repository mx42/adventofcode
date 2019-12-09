module Day5 (day5) where

import Intcode

day5 :: IO ()
day5 = do
  putStrLn "AoC 2019 day 5"
  putStr "Input >"
  input <- getLine
  putStrLn ""

  let intCodes = parseProgram input

  let (_, outputP1) = computer [1] intCodes [] 0
  putStrLn ("Part1: " ++ show (outputP1 !! 0))

  let (_, outputP2) = computer [5] intCodes [] 0
  putStrLn ("Part2: " ++ show (outputP2 !! 0))
