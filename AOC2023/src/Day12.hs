module Day12 (day12) where

import Data.List (intercalate)
import Data.List.Split (splitOn)

parseLine :: String -> (String, [Int])
parseLine input = (head parts, parsedPart2)
  where parts = splitOn " " input
        parsedPart2 = map read (splitOn "," $ parts !! 1)

computeLine :: String -> [Int]
computeLine input = reverse $ snd $ foldl accumulate ('.', []) input
  where accumulate (_, counts) '.' = ('.', counts)
        accumulate ('#', h:t) '#' = ('#', h + 1:t)
        accumulate (_, counts) '#' = ('#', 1:counts)
        accumulate (_, counts) c = (c, counts)

bruteforceQmarks :: String -> [String]
bruteforceQmarks "" = [""]
bruteforceQmarks ('?':t) = [ c:xs | c <- ['.', '#'], xs <- bruteforceQmarks t]
bruteforceQmarks (h:t) = [ h:xs | xs <- bruteforceQmarks t]

getPossibleEntries :: String -> [Int] -> [String]
getPossibleEntries input check = filter ((== check) . computeLine) possibilities
  where possibilities = bruteforceQmarks input

adaptForP2 :: (String, [Int]) -> (String, [Int])
adaptForP2 (str, check) = (repeatedStr, repeatedCheck)
  where repeatedStr = intercalate "?" $ replicate 5 str
        repeatedCheck = concat $ replicate 5 check

day12 :: IO ()
day12 = do
  putStrLn "AOC 2023 day 12"
  input <- getContents
  let input' = map parseLine $ lines input
  putStrLn "Part1"
  let computedPossibilities = map (\(str, check) -> length (getPossibleEntries str check)) input'
  print $ sum computedPossibilities
  putStrLn "Part2"
  let inputP2 = map adaptForP2 input'
  let possP2 = map (\(str, check) -> length (getPossibleEntries str check)) inputP2
  print $ sum possP2
