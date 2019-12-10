module Day9 (day9) where

import Intcode

day9 :: IO ()
day9 = do
  putStrLn $ "AoC 2019 day 9"
  input <- getLine
  let memory = parseProgram input

  let outputP1 = runProgramV2 [1] memory
  putStrLn $ "Part1: " ++ show outputP1
