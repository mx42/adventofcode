module Day7 (day7) where

import Data.List
import Intcode

processProgram :: [Int] -> [Int] -> Int
processProgram program inputs = head outputs
  where (_, outputs) = computer inputs program [] 0

chainProcesses :: [Int] -> [Int] -> Int -> Int
chainProcesses program (phase:phases) signal
  | null phases = newSignal
  | otherwise = chainProcesses program phases newSignal
  where newSignal = processProgram program [phase, signal]

testCombinations :: [Int] -> [([Int], Int)]
testCombinations program = map (\p -> (p, chainProcesses program p 0)) phasesPerm
  where phasesPerm = permutations [0..4]

day7 :: IO ()
day7 = do
  putStrLn $ "AoC 2019 day 7"
  input <- getLine

  let program = parseProgram input
  let combinations = testCombinations program

  let p1 = maximumBy (\(_, a) (_, b) -> compare a b) combinations

  putStrLn $ "Part 1: " ++ (show p1)
