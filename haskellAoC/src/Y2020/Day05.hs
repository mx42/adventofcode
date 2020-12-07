module Y2020.Day05 (y20day05) where

import Data.List

parseRow :: [Char] -> Int
parseRow ('B':[]) = 1
parseRow ('F':[]) = 0
parseRow ('B':xs) = 1 + 2 * parseRow xs
parseRow ('F':xs) = 2 * parseRow xs

parseCol :: [Char] -> Int
parseCol "RRR" = 7
parseCol "RRL" = 6
parseCol "RLR" = 5
parseCol "RLL" = 4
parseCol "LRR" = 3
parseCol "LRL" = 2
parseCol "LLR" = 1
parseCol "LLL" = 0

parseInput :: [Char] -> (Int, Int)
parseInput input = (row, col)
  where row = parseRow $ reverse $ (take 7 input)
        col = parseCol $ drop 7 input

computeSeatId :: (Int, Int) -> Int
computeSeatId (row, col) = row * 8 + col

findMissingId :: [Int] -> Int
findMissingId (x:x2:xs)
  | x2 == x + 1 = findMissingId (x2:xs)
  | otherwise = x + 1
findMissingId _ = -1

y20day05 :: [String] -> (String, String)
y20day05 input = (part1, part2)
  where part1 = show $ maximum $ map computeSeatId parsedInput
        part2 = show $ findMissingId $ sort $ map computeSeatId parsedInput
        parsedInput = map parseInput input
