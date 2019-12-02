import Data.List

move :: [(Int, Int)] -> Char -> [(Int, Int)]
move ((x, y):xs) '>' = (x + 1, y):((x, y):xs)
move ((x, y):xs) '<' = (x - 1, y):((x, y):xs)
move ((x, y):xs) '^' = (x, y + 1):((x, y):xs)
move ((x, y):xs) 'v' = (x, y - 1):((x, y):xs)

houses :: [Char] -> [(Int, Int)]
houses input = foldl move [(0, 0)] input

foldlBoth :: (a -> b -> a) -> (a, a) -> [b] -> (a, a)
foldlBoth f (l1, l2) (x1:(x2:xs)) = foldlBoth f (f l1 x1, f l2 x2) xs
foldlBoth f (l1, l2) (x1:_) = foldlBoth f (f l1 x1, l2) []
foldlBoth f (l1, l2) [] = (l1, l2)

housesWithBot :: [Char] -> [(Int, Int)]
housesWithBot input = santaHouses ++ botHouses
  where (santaHouses, botHouses) = foldlBoth move ([(0, 0)], [(0, 0)]) input

uniqueHouses :: [(Int, Int)] -> [((Int, Int), Int)]
uniqueHouses input = (map (\xs -> (head xs, length xs)) . group . sort) input

day3part1 :: [Char] -> Int
day3part1 input = length (uniqueHouses (houses input))

day3part2 :: [Char] -> Int
day3part2 input = length (uniqueHouses (housesWithBot input))
