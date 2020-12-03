module Y2015.Day02 (y15day02) where

import Data.List.Split

computeSides :: [Int] -> [Int]
computeSides dims = [l * w, w * h, h * l]
  -- Not really safe...
  where l = dims !! 0
        w = dims !! 1
        h = dims !! 2

computeWrapping :: [Int] -> Int
computeWrapping dims = sum (map (* 2) sides)
  where sides = computeSides dims

computeSlack :: [Int] -> Int
computeSlack dims = minimum sides
  where sides = computeSides dims

computeRibbonWrap :: [Int] -> Int
computeRibbonWrap dims = (sum dims - maximum dims) * 2

computeRibbonBow :: [Int] -> Int
computeRibbonBow dims = product dims

parseDims :: String -> [Int]
parseDims input = map read (splitOn "x" input)

part1computation :: [Int] -> Int
part1computation dims = computeWrapping dims + computeSlack dims

part2computation :: [Int] -> Int
part2computation dims = computeRibbonWrap dims + computeRibbonBow dims

y15day02 :: [String] -> (String, String)
y15day02 input = (part1, part2)
  where part1 = show $ compute part1computation
        part2 = show $ compute part2computation
        compute fn = sum $ map fn $ map parseDims input
