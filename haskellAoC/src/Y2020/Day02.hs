module Y2020.Day02 (y20day02) where

import Data.List.Split

parseInput :: String -> (Int, Int, Char, String)
parseInput input = (qty 0, qty 1, (parts !! 1) !! 0, parts !! 2)
  where parts = splitOn " " $ filter (\c -> not $ elem c ":") input
        qty n = read $ (!! n) $ splitOn "-" $ (parts !! 0)

isValidPassP1 :: (Int, Int, Char, String) -> Bool
isValidPassP1 (minP, maxP, c, pass) = count >= minP && count <= maxP
  where count = length $ filter (== c) pass

isValidPassP2 :: (Int, Int, Char, String) -> Bool
isValidPassP2 (pos1, pos2, c, pass) = isPos pos1 `xor` isPos pos2
  where isPos p = (pass !! (p - 1)) == c
        xor a b = (a || b) && (a /= b)

y20day02 :: [String] -> (String, String)
y20day02 input = (part1, part2)
  where
    entries = map parseInput $ input
    strCount f xs = show $ length $ filter f xs
    part1 = strCount isValidPassP1 entries
    part2 = strCount isValidPassP2 entries
