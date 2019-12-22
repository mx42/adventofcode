module Day13 (day13) where

import Intcode

parseOutput :: [Int] -> [(Int, Int, Int)]
parseOutput (x:(y:(t:xs))) = (x, y, t):(parseOutput xs)
parseOutput _ = []

day13 :: IO ()
day13 = do
  putStrLn $ "AoC 2019 day 13"
  input <- getLine
  let memory = parseProgram input

  let outputP1 = parseOutput $ runProgramV2 [] memory

  putStrLn $ "Part1: " ++ (show $ length $ filter (\(_,_,t) -> t == 2) outputP1)

  -- let hackedMemory = 2:xs
  --      where (_:xs) = memory

  -- let outputP2 = parseOutput $ runProgramV2 [] hackedMemory
