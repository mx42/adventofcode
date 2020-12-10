module Y2020.Day10 (y20day10) where

import Data.List

sliding :: [Int] -> [(Int, Int)]
sliding (x1:x2:xs) = ((x1, x2):(sliding (x2:xs)))
sliding (x1:[]) = ((x1, x1+3):[])
sliding [] = []

combinations :: Int -> Int
combinations n = nb !! n
  where nb = [1, 1, 2, 4, 7, 13]

y20day10 :: [String] -> (String, String)
y20day10 input = (part1, part2)
  where part1 = show $ product $ map length $ group $ filter (/= 2) $ sort $ diffs
        part2 = show $ product $ map (combinations . length) $ filter (\(x:_) -> x /= 3) $ group $ diffs
        input' = sort $ map read input :: [Int]
        diffs = map (\(a, b) -> b - a) $ (0, head input'):(sliding input')
