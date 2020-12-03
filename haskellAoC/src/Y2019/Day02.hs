module Y2019.Day02 (day2) where

import Y2019.Intcode

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs

computeVerbNoun :: Int -> Int -> [Int] -> Int
computeVerbNoun noun verb input = (runProgramV1 newInput) !! 0
  where newInput = replaceNth 1 noun . replaceNth 2 verb $ input

bruteforce :: Int -> Int -> [Int] -> Int -> Int
bruteforce noun verb input objective
  | result == objective = noun * 100 + verb
  | verb == 99 = bruteforce (noun + 1) 0 input objective
  | noun < 100 = bruteforce noun (verb + 1) input objective
  where result = computeVerbNoun noun verb input

day2 :: [String] -> (String, String)
day2 (input:_) = (part1, part2)
  where intCodes = parseProgram input
        part1 = show $ computeVerbNoun 12 2 intCodes
        objective = 19690720
        part2 = show $ bruteforce 0 0 intCodes objective
