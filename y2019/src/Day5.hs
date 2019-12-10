module Day5 (day5) where

import Intcode

day5 :: IO ()
day5 = do
  putStrLn "AoC 2019 day 5"
  putStr "Input >"
  input <- getLine
  putStrLn ""

  let intCodes = parseProgram input

  let outputP1 = runProgramV2 [1] intCodes
  putStrLn ("Part1: " ++ show (outputP1 !! 0))

  let outputP2 = runProgramV2 [5] intCodes
  putStrLn ("Part2: " ++ show (outputP2 !! 0))
