module Day4 (day4) where

import Data.List.Split
import Data.List

isValid :: (Int -> Bool) -> Int -> Bool
isValid groupsCriteria n = length6 && isSorted && hasGroups
  where digits = show n
        length6 = (length digits) == 6
        isSorted = digits == sort digits
        hasGroups = any (\d -> groupsCriteria (length d)) (group digits)

getNext :: (Int -> Bool) -> Int -> Int
getNext groupsCriteria cur
  | isValid groupsCriteria (cur + 1) = (cur + 1)
  | otherwise = getNext groupsCriteria (cur + 1)

day4 :: IO ()
day4 = do
  putStrLn "AoC 2019 day 4"
  putStr "Enter input >"
  input <- getLine
  putStrLn ""

  let range = (map read (splitOn "-" input)) :: [Int]
  putStrLn ("Range is " ++ show (range !! 0) ++ " to " ++ show (range !! 1))

  let iterP1 = takeWhile (< (range !! 1)) (iterate (getNext (> 1)) (range !! 0))
  putStrLn ("Part1: " ++ show ((length iterP1) - 1))

  let iterP2 = takeWhile (< (range !! 1)) (iterate (getNext (== 2)) (range !! 0))
  putStrLn ("Part2: " ++ show ((length iterP2) - 1))
