module Day04 (day04) where

import Data.List (intersect)
import Data.List.Split (splitOn)

parseSections :: String -> [Int]
parseSections input = [startInt..endInt]
  where (start : end : _) = splitOn "-" input
        startInt = read start :: Int
        endInt = read end :: Int


parseLine :: String -> ([Int], [Int])
parseLine input = (sections1, sections2)
  where sections1 = parseSections $ head parts
        sections2 = parseSections $ last parts
        parts = splitOn "," input

completelyOverlap :: ([Int], [Int]) -> Bool
completelyOverlap (s1, s2) = overlapLen == s1Len || overlapLen == s2Len
  where overlap = s1 `intersect` s2
        overlapLen = length overlap
        s1Len = length s1
        s2Len = length s2

doOverlap :: ([Int], [Int]) -> Bool
doOverlap (s1, s2) = not . null $ s1 `intersect` s2

day04 :: IO ()
day04 = do
  putStrLn "AoC 2022 day 4"
  input <- getContents
  let parsed = map parseLine $ lines input
  putStrLn "Part1"
  let resP1 = length $ filter completelyOverlap parsed
  print resP1
  putStrLn "Part2"
  let resP2 = length $ filter doOverlap parsed
  print resP2