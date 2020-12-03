module Y2019.Day05 (day5) where

import Y2019.Intcode

day5 :: [String] -> (String, String)
day5 (input:_) = (part1, part2)
  where intCodes = parseProgram input
        outputP1 = runProgramV2 [1] intCodes
        part1 = show $ outputP1 !! 0
        outputP2 = runProgramV2 [5] intCodes
        part2 = show $ outputP2 !! 0
