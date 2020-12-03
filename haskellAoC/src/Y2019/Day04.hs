module Y2019.Day04 (day4) where

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

day4 :: [String] -> (String, String)
day4 (input:_) = (part1, part2)
  where range = (map read (splitOn "-" input)) :: [Int]
        iterP1 = takeWhile (< (range !! 1)) (iterate (getNext (> 1)) (range !! 0))
        part1 = show ((length iterP1) - 1)
        iterP2 = takeWhile (< (range !! 1)) (iterate (getNext (== 2)) (range !! 0))
        part2 = show ((length iterP2) - 1)
