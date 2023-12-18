module Day03 (day03) where

import Data.List (intersect)
import Data.List.Split (chunksOf)
import Data.Char (ord)

getCommonP1 :: String -> Char
getCommonP1 input = let
    part1 = take half input
    part2 = drop half input
    half = div (length input) 2
  in (intersect part1 part2) !! 0

getCommonP2 :: [[Char]] -> Char
getCommonP2 [] = '!'
getCommonP2 (h:t) = (foldr intersect h t) !! 0

getPriority :: Char -> Int
getPriority c
  | c >= 'a' && c <= 'z' = ord c - ord 'a' + 1
  | c >= 'A' && c <= 'Z' = ord c - ord 'A' + 27
  | otherwise = 0

day03 :: IO ()
day03 = do
  putStrLn "AoC 2022 day 3"
  input <- getContents
  putStrLn "Part1"
  let resP1 = sum $ map (getPriority . getCommonP1) (lines input)
  print resP1
  putStrLn "Part2"
  let resP2 = sum $ map (getPriority . getCommonP2) $ chunksOf 3 (lines input)
  print resP2
