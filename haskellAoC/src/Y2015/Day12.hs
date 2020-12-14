module Y2015.Day12 (y15day12) where

import Data.Char

-- getToNext :: Char -> String -> String
-- getToNext _ [] = []
-- getToNext c (x:xs)
--   | x == c = []
--   | otherwise = x:(getToNext c xs)

getStructureSum :: String -> Int
getStructureSum s = sum $ map read $ words $ map keepNum s
  where keepNum '-' = '-'
        keepNum c
          | isDigit c = c
          | otherwise = ' '

y15day12 :: [String] -> (String, String)
y15day12 (input:_) = (part1, part2)
  where part1 = show $ getStructureSum input
        part2 = show $ "WIP"
