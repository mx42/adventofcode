module Y2015.Day11 (y15day11) where

import Data.Char
import Data.List

allTrue :: [(a -> Bool)] -> a -> Bool
allTrue predicates entry = all (== True) $ map ($ entry) predicates

rowOfThree :: String -> Bool
rowOfThree (x1:x2:x3:xs)
  | c3 == (c2 + 1) && c2 == (c1 + 1) = True
  | otherwise = rowOfThree (x2:x3:xs)
  where c1 = ord x1
        c2 = ord x2
        c3 = ord x3
rowOfThree _ = False

-- Do we accept triples as 1 pair ?
twoPairs :: String -> Bool
twoPairs s = (>= 2) . length $ filter (== 2) $ map length $ group s

isValid :: String -> Bool
isValid s = allTrue [validChars, length8, rowOfThree, twoPairs] s
  where validChars = all (\c -> isLower c && (c `notElem` "iol"))
        length8 = (== 8) . length

nextSequence :: String -> String
nextSequence s = reverse $ nextSequence' $ reverse $ s
  where nextSequence' ('z':cs) = 'a':nextSequence' cs
        nextSequence' (c:cs) = ((chr . (+1) . ord) $ c) : cs
        nextSequence' [] = []

y15day11 :: [String] -> (String, String)
y15day11 (input:_) = (part1, part2)
  where [part1, part2] = take 2 $ filter isValid $ iterate nextSequence input
