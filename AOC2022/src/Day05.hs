module Day05 (day05) where

import Data.List.Split (splitOn, chunksOf)

updateAcc :: [[Char]] -> String -> [[Char]]
updateAcc acc line = acc ++ newItems
  where newItems = map (take 1) $ chunksOf 4 line

parsePart1 :: [[Char]] -> [String] -> [[Char]]
parsePart1 acc [] = acc
parsePart1 acc (h:t) = parsePart1 updatedAcc t
  where updatedAcc = updateAcc acc (drop 1 h)

day05 :: IO ()
day05 = do
  putStrLn "AoC 2022 day 5"
  input <- getContents
  putStrLn "Part1"
  let inputParts = splitOn "\n\n" input
  let inputPart1 = parsePart1 [] $ drop 1 $ reverse $ lines $ head inputParts
  let inputPart2 = lines $ last inputParts
  putStrLn "input p1"
  print $ drop 1 $ reverse $ lines $ head inputParts
  print inputPart1
  putStrLn "input p2"
  print inputPart2
