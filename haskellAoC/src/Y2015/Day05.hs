module Y2015.Day05 (y15day05) where

has3vowels :: String -> Bool
has3vowels input = (>= 3) $ length $ filter (`elem` "aeiou") input

hasArepeat :: String -> Bool
hasArepeat (x:x2:xs)
  | x == x2 = True
  | otherwise = hasArepeat (x2:xs)
hasArepeat _ = False

noForbiddenStr :: String -> Bool
noForbiddenStr (x:x2:xs)
  | (x:[x2]) `elem` ["ab", "cd", "pq", "xy"] = False
  | otherwise = noForbiddenStr (x2:xs)
noForbiddenStr _ = True

contains :: String -> String -> Bool
contains _ [] = True
contains [] _ = False
contains (x:xs) (y:ys)
  | x == y = contains xs ys
  | otherwise = contains xs (y:ys)

repeatedTwoLetters :: String -> Bool
repeatedTwoLetters (x1:x2:xs)
  | xs `contains` (x1:[x2]) = True
  | otherwise = repeatedTwoLetters (x2:xs)
repeatedTwoLetters _ = False

aRepeatAcrossOne :: String -> Bool
aRepeatAcrossOne (x1:x2:x3:xs)
  | x1 == x3 = True
  | otherwise = aRepeatAcrossOne (x2:x3:xs)
aRepeatAcrossOne _ = False

y15day05 :: [String] -> (String, String)
y15day05 input = (part1, part2)
  where
    part1rules = [has3vowels, hasArepeat, noForbiddenStr]
    part2rules = [repeatedTwoLetters, aRepeatAcrossOne]
    applyRules rules pwd = all (== True) $ map ($ pwd) rules
    countCorrectPwd rules = length $ filter (== True) $ map (applyRules rules) input
    part1 = show $ countCorrectPwd part1rules
    part2 = "WIP, invalid: " ++ (show $ countCorrectPwd part2rules)
