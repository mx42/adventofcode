module Day7 (day7) where

import Data.List
import Intcode

processProgram :: [Int] -> [Int] -> Int
processProgram program inputs = head outputs
  where outputs = runProgramV2 inputs program

-- Memory -> phase (1st input) -> 2nd input -> outputs
chainProcesses :: [Int] -> [Int] -> Int -> Int
chainProcesses program (phase:nextPhases) signal
  | (null nextPhases) = newSignal
  | otherwise = chainProcesses program nextPhases newSignal
  where newSignal = processProgram program [phase, signal]

testCombinationsP1 :: [Int] -> [([Int], Int)]
testCombinationsP1 program = map (\p -> (p, chainProcesses program p 0)) phasesPerm
  where phasesPerm = permutations [0..4]

testCombinationsP2 :: [Int] -> [([Int], Int)]
testCombinationsP2 program = map (\p -> (p, runProgramV3 program p)) phasesPerm
  where phasesPerm = permutations [5..9]

day7 :: IO ()
day7 = do
  putStrLn $ "AoC 2019 day 7"
  input <- getLine

  let program = parseProgram input
  let combinationsP1 = testCombinationsP1 program

  let p1 = maximumBy (\(_, a) (_, b) -> compare a b) combinationsP1

  putStrLn $ "Part 1: " ++ (show p1)

  -- WIP
  -- let combinationsP2 = testCombinationsP2 program
  -- let p2 = maximumBy (\(_, a) (_, b) -> compare a b) combinationsP2

  -- putStrLn $ "Part 2 (WIP/Buggy): " ++ (show p2)
