{-# LANGUAGE TupleSections #-}

module Day04 (day04) where

import Data.List (intersect)
import Data.List.Split (splitOn)
import qualified Data.Map as Map

parseLine :: String -> Int
parseLine input = length matchingNumbers
  where (leftSide: part3: _) = splitOn " | " input
        (_: part2: _) = splitOn ": " leftSide
        winningNumbers =  filter (not . null) $ splitOn " " part2
        cardNumbers = filter (not . null) $ splitOn " " part3
        matchingNumbers = winningNumbers `intersect` cardNumbers

computeCardScore :: Int -> Int
computeCardScore 0 = 0
computeCardScore nbMatching = 2 ^ nbMatching - 1

winCards :: ([Int], [(Int, Int)]) -> Int -> ([Int], [(Int, Int)])
winCards (cardsQty, []) _ = (cardsQty, [])
winCards (cardsQty, (nb, matching):t) _ = (nb:cardsQty, updatedNext)
  where updatedNext = if matching == 0 then t else (map (\(qty, m) -> (qty + nb, m)) $ take matching t) ++ drop matching t


day04 :: IO ()
day04 = do
   putStrLn "AOC 2023 day 04"
   input <- getContents
   let cards = map parseLine $ lines input
   let resP1 = sum $ map computeCardScore cards
   putStrLn "Part1"
   print resP1
   let cardsWithQty = map (1,) cards
   putStrLn "Part2"
   let resP2 = sum $ fst $ foldl winCards ([], cardsWithQty) cards
   print resP2