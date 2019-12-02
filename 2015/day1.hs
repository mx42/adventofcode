import Data.List

day1steps :: [Char] -> [Int]
day1steps xs = [if x == '(' then 1 else -1 | x <- xs]

day1p1 :: [Char] -> Int
day1p1 xs = sum(day1steps xs)

day1p2 :: [Char] -> Maybe Int
day1p2 xs = findIndex (== entrance) steps
  where steps = scanl (\acc x -> acc + x) 0 (day1steps xs)
        entrance = -1
