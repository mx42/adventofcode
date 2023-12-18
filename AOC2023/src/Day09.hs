module Day09 (day09) where

import Data.List.Split (splitOn)

parseLine :: String -> [Int]
parseLine i = map read $ splitOn " " i

getDifferences :: [Int] -> [Int]
getDifferences [] = []
getDifferences [_] = []
getDifferences (h:t@(th:_)) = (th - h) : getDifferences t

listIsZeros :: [Int] -> Bool
listIsZeros = all (== 0)

getIterations :: [Int] -> [[Int]]
getIterations a = takeWhile (not . listIsZeros) $ iterate getDifferences a

getNextItem :: [Int] -> Int -> Int
getNextItem l incr = last l + incr

getNextItems :: [[Int]] -> Int
getNextItems = foldr getNextItem 0

getPrevItem :: [Int] -> Int -> Int
getPrevItem l incr = head l - incr

getPrevItems :: [[Int]] -> Int
getPrevItems = foldr getPrevItem 0

day09 :: IO ()
day09 = do
  putStrLn "AOC 2023 day 09"
  input <- getContents
  let input' = map parseLine $ lines input
  putStrLn "Part1"
  print $ sum $ map (getNextItems . getIterations) input'
  putStrLn "Part2"
  print $ sum $ map (getPrevItems . getIterations) input'
