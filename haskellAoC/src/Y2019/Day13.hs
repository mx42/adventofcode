module Y2019.Day13 (day13) where

import Y2019.Intcode

parseOutput :: [Int] -> [(Int, Int, Int)]
parseOutput (x:(y:(t:xs))) = (x, y, t):(parseOutput xs)
parseOutput _ = []

day13 :: [String] -> (String, String)
day13 (input:_) = (part1, part2)
  where memory = parseProgram input
        outputP1 = parseOutput $ runProgramV2 [] memory
        part1 = show $ length $ filter (\(_,_,t) -> t == 2) outputP1
        part2 = "WIP"

  -- let hackedMemory = 2:xs
  --      where (_:xs) = memory

  -- let outputP2 = parseOutput $ runProgramV2 [] hackedMemory
