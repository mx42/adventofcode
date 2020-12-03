module Y2020.Day01 (y20day01) where

import Data.Maybe
import Data.Sort

findPair :: Int -> Int -> [Int] -> Maybe (Int, Int)
findPair target x (h:t)
  | target == x + h = Just (x, h)
  | target < x + h = Nothing
  | otherwise = findPair target x t
findPair _ _ [] = Nothing

findFirstMatchingPair :: Int -> [Int] -> Maybe (Int, Int)
findFirstMatchingPair target (x:xs)
  | isJust res = res
  | otherwise = findFirstMatchingPair target (xs)
  where res = findPair target x xs
findFirstMatchingPair _ _ = Nothing

findFirstMatchingTriplet :: Int -> [Int] -> (Int, Int, Int)
findFirstMatchingTriplet target (x:xs)
 | isJust res = let Just (a, b) = res in (x, a, b)
 | otherwise = findFirstMatchingTriplet target xs
 where res = findFirstMatchingPair (target - x) xs

y20day01 :: [String] -> (String, String)
y20day01 input = (part1, part2)
  where
    entries = (sort (map read input)) :: [Int]
    Just (a, b) = findFirstMatchingPair 2020 entries
    (x, y, z) = findFirstMatchingTriplet 2020 entries
    part1 = show (a * b)
    part2 = show (x * y * z)
