module Y2015.Day17 (y15day17) where

import Data.List

getCombinations :: Int -> [Int] -> [[Int]]
getCombinations _ [] = []
getCombinations tgt (nb:[])
  | nb == tgt = [[nb]]
  | otherwise = []
getCombinations tgt (nb:t)
  | nb > tgt  = []
  | nb == tgt = [[nb]] ++ nextWithoutNb
  | otherwise = nextWithNb ++ nextWithoutNb
      where nextWithNb    = map (\c -> [nb] ++ c) $ getCombinations (tgt - nb) t
            nextWithoutNb = getCombinations tgt t

y15day17 :: [String] -> (String, String)
y15day17 input = (part1, part2)
  where part1  = show $ length $ combs
        part2  = show $ length $ head $ group $ sort $ map (length) $ combs

        numbers = sort $ map read input :: [Int]
        combs   = getCombinations 150 numbers
