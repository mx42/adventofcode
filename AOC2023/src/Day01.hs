module Day01 (day01) where

import Data.Char (digitToInt, isDigit)

getNumberFromDigits :: [Int] -> Int
getNumberFromDigits nums = head nums * 10 + last nums

extractDataP1 :: String -> Int
extractDataP1 input = getNumberFromDigits $ map digitToInt (filter isDigit input)

extractDataP2 :: String -> Int
extractDataP2 input = getFirst input * 10 + getSecond (reverse input)
  where
    getFirst ('o':'n':'e':_) = 1
    getFirst ('t':'w':'o':_) = 2
    getFirst ('t':'h':'r':'e':'e':_) = 3
    getFirst ('f':'o':'u':'r':_) = 4
    getFirst ('f':'i':'v':'e':_) = 5
    getFirst ('s':'i':'x':_) = 6
    getFirst ('s':'e':'v':'e':'n':_) = 7
    getFirst ('e':'i':'g':'h':'t':_) = 8
    getFirst ('n':'i':'n':'e':_) = 9
    getFirst (c:t)
      | isDigit c = digitToInt c
      | otherwise = getFirst t
    getFirst [] = 0    
    getSecond ('e':'n':'o':_) = 1
    getSecond ('o':'w':'t':_) = 2
    getSecond ('e':'e':'r':'h':'t':_) = 3
    getSecond ('r':'u':'o':'f':_) = 4
    getSecond ('e':'v':'i':'f':_) = 5
    getSecond ('x':'i':'s':_) = 6
    getSecond ('n':'e':'v':'e':'s':_) = 7
    getSecond ('t':'h':'g':'i':'e':_) = 8
    getSecond ('e':'n':'i':'n':_) = 9
    getSecond (c:t)
      | isDigit c = digitToInt c
      | otherwise = getSecond t
    getSecond [] = 0    

day01 :: IO ()
day01 = do
   putStrLn "AOC 2023 day 01"
   input <- getContents
   putStrLn "Part1"
   let resP1 = map extractDataP1 $ lines input
   print $ sum resP1
   putStrLn "Part2"
   let resP2 = map extractDataP2 $ lines input
   print resP2
