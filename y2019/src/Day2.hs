module Day2 (day2) where

import Intcode

computeVerbNoun :: Int -> Int -> [Int] -> Int
computeVerbNoun noun verb input = (fst (computer [] newInput [] 0)) !! 0
  where newInput = replaceNth 1 noun . replaceNth 2 verb $ input

bruteforce :: Int -> Int -> [Int] -> Int -> Int
bruteforce noun verb input objective
  | result == objective = noun * 100 + verb
  | verb == 99 = bruteforce (noun + 1) 0 input objective
  | noun < 100 = bruteforce noun (verb + 1) input objective
  where result = computeVerbNoun noun verb input

day2 :: IO ()
day2 = do
  putStrLn "AoC 2019 day 2"
  putStr "Enter input >"
  input <- getLine
  putStrLn ""
  let intCodes = parseProgram input

  putStr "Part 1: "
  print (computeVerbNoun 12 2 intCodes)

  putStr "Part 2: "
  let objective = 19690720
  print (bruteforce 0 0 intCodes objective)
