module Y2015.Day09 (y15day09) where

import Data.List.Split

parseInput :: String -> (String, String, Int)
parseInput input = (city1, city2, dist)
  where (city1:split1:_) = splitOn " to " input
        (city2:raw_dist:_) = splitOn " = " split1
        dist = read $ raw_dist

y15day09 :: [String] -> (String, String)
y15day09 input = (part1, part2)
  where part1 = show $ map parseInput input
        part2 = "WIP"
