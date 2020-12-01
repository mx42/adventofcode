module Day01 (day01) where

import Data.Maybe
import Data.Sort
import Debug.Trace


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

day01 :: IO ()
day01 = do
  putStrLn "AoC 2020 day 1"
  input <- getContents
  let entries = (sort (map read (lines input))) :: [Int]

  let Just (a, b) = findFirstMatchingPair 2020 entries
  putStrLn $ "Part 1: " ++ (show (a * b))

  let (x, y, z) = findFirstMatchingTriplet 2020 entries
  putStrLn $ "Part 2: " ++ (show (x * y * z))
