module Y2020.Day09 (y20day09) where

import Data.Maybe

hasSumInList :: Int -> [Int] -> Bool
hasSumInList _ [] = False
hasSumInList nb (x:xs)
  | (nb - x) `elem` xs = True
  | otherwise = hasSumInList nb xs

walkTilInvalid :: [Int] -> [Int] -> Int
walkTilInvalid (cur:next) last25
  | hasSumInList cur last25 = walkTilInvalid next (cur:(init last25))
  | otherwise = cur
walkTilInvalid _ _ = -1

getSum :: [Int] -> Int -> Maybe [Int]
getSum [] _ = Nothing
getSum (y:ys) t
  | y > t = Nothing
  | y == t = Just [y]
  | otherwise = fmap (\zs -> y:zs) (getSum ys (t-y))

findContiguousSum :: [Int] -> Int -> [Int]
findContiguousSum (x:xs) target
  | x >= target = findContiguousSum xs target
  | isJust sumStartingThere = fromJust sumStartingThere
  | otherwise = findContiguousSum xs target
  where sumStartingThere = getSum xs target

y20day09 :: [String] -> (String, String)
y20day09 input = (part1, part2)
  where part1 = show $ firstInvalid
        part2 = show $ (minimum contiguousSum + maximum contiguousSum)
        input' = map read $ input :: [Int]
        preamble = reverse $ take 25 $ input'
        rest = drop 25 $ input'
        firstInvalid = walkTilInvalid rest preamble
        contiguousSum = findContiguousSum input' firstInvalid
