module Y2019.Day09 (day9) where

import Y2019.Intcode

day9 :: [String] -> (String, String)
day9 (input:_) = (part1, part2)
  where memory = parseProgram input
        part1 = show $ runProgramV2 [1] memory
        part2 = show $ runProgramV2 [2] memory
