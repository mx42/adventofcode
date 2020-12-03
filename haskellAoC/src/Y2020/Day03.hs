module Y2020.Day03 (y20day03) where

walkMap :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
walkMap (w, h) (wstep, hstep) = zip wCoords hCoords
  where wCoords = map (`mod` w) $ take h $ iterate (+ wstep) 0
        hCoords = take h $ iterate (+ hstep) 0

getCharAtPos :: [String] -> (Int, Int) -> Char
getCharAtPos myMap (w, h)
  | h <= length myMap = (myMap !! h) !! w
  | otherwise = '\0'

getNumberOfTrees :: [String] -> [(Int, Int)] -> Int
getNumberOfTrees myMap coords = length $ filter (== '#') $ map (getCharAtPos myMap) coords

y20day03 :: [String] -> (String, String)
y20day03 input = (part1, part2)
  where width = length $ input !! 0
        height = (length $ input)
        getNb slope = getNumberOfTrees input $ walkMap (width, height) slope
        part1 = show $ getNb (3, 1)
        part2 = show $ product $ map getNb [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
