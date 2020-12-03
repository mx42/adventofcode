module Y2015.Day01 (y15day01) where

import Data.List

day1steps :: [Char] -> [Int]
day1steps xs = [if x == '(' then 1 else -1 | x <- xs]

day1p1 :: [Char] -> Int
day1p1 xs = sum(day1steps xs)

day1p2 :: [Char] -> Maybe Int
day1p2 xs = findIndex (== entrance) steps
  where steps = scanl (\acc x -> acc + x) 0 (day1steps xs)
        entrance = -1

y15day01 :: [String] -> (String, String)
y15day01 (input:_) = (part1, part2)
  where part1 = show $ day1p1 input
        part2 = show $ day1p2 input
