module Day2
  ( day2
  ) where

import Data.List.Split

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs

processInput :: [Int] -> Int -> [Int]
processInput input index
  | opcode == 1 = let operand1 = input !! (input !! (index + 1))
                      operand2 = input !! (input !! (index + 2))
                      resultIndex = input !! (index + 3)
                      result = operand1 + operand2
                      newInput = replaceNth resultIndex result input
                  in processInput newInput newIndex
  | opcode == 2 = let operand1 = input !! (input !! (index + 1))
                      operand2 = input !! (input !! (index + 2))
                      resultIndex = input !! (index + 3)
                      result = operand1 * operand2
                      newInput = replaceNth resultIndex result input
                  in processInput newInput newIndex
  | opcode == 99 = input
  | opcode > 0 = input
  where opcode = input !! index
        newIndex = index + 4

computeVerbNoun :: Int -> Int -> [Int] -> Int
computeVerbNoun noun verb input = (processInput newInput 0) !! 0
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
  let intCodes = map (read :: String -> Int) (splitOn "," input)

  putStr "Part 1: "
  print (computeVerbNoun 12 2 intCodes)

  putStr "Part 2: "
  let objective = 19690720
  print (bruteforce 0 0 intCodes objective)
