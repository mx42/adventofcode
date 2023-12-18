module Day06 (day06) where

import Data.List.Split (splitOn)

parseInput :: [String] -> [(Int, Int)]
parseInput input = zip (head cleaned) (cleaned !! 1)
  where input_ = map (last . splitOn ":") input :: [String]
        cleanPart i = map read $ filter (/= "") $ splitOn " " i :: [Int]
        cleaned = map cleanPart input_ :: [[Int]]

parseInputP2 :: [String] -> (Int, Int)
parseInputP2 input = (head cleaned, cleaned !! 1)
  where input_ = map (last . splitOn ":") input :: [String]
        cleanPart i = read $ filter (/= ' ') i :: Int
        cleaned = map cleanPart input_ :: [Int]

runDistances :: Int -> [(Int, Int)]
runDistances totalTime = forChargeTime totalTime
  where forChargeTime 0 = [(0, 0)]
        forChargeTime n = (n, (totalTime - n) * n) : forChargeTime (n - 1)

day06 :: IO ()
day06 = do
  putStrLn "AOC 2023 day 06"
  input <- getContents
  let inputP1 = parseInput $ lines input
  let winningRuns = map (\i -> filter (\(_, d) -> d > snd i) $ runDistances $ fst i) inputP1
  putStrLn "Part1"
  let resP1 = product $ map length winningRuns
  print resP1
  putStrLn "Part2"
  let inputP2 = parseInputP2 $ lines input
  let resP2 = length $ filter (\(_, d) -> d > snd inputP2) $ runDistances $ fst inputP2
  print resP2
