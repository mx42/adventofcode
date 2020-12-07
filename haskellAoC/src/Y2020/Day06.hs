module Y2020.Day06 (y20day06) where

import Data.List.Split
import Data.List

countUniques :: String -> Int
countUniques input = uniq
  where uniq = length $ group $ sort input'
        input' = filter (/= '\n') input

keepCommons :: [Char] -> [Char] -> [Char]
keepCommons [] _ = []
keepCommons _ [] = []
keepCommons (x:xs) (y:ys)
  | x == y = x:(keepCommons xs ys)
  | x < y = keepCommons xs (y:ys)
  | x > y = keepCommons (x:xs) ys

countDups :: String -> Int
countDups input = length $ dups
  where (h:t) = map sort $ lines input
        dups = foldl keepCommons h t

y20day06 :: [String] -> (String, String)
y20day06 input = (part1, part2)
  where part1 = show $ sum $ map countUniques groups
        part2 = show $ sum $ map countDups groups
        groups = splitOn "\n\n" $ unlines input
