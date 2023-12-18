module Day02 (day02) where

parseInput :: String -> [(Char, Char)]
parseInput input = map parseLine (lines input)
  where parseLine :: String -> (Char, Char)
        parseLine (c1 : ' ' : c2 : _) = (c1, c2)
        parseLine _ = error "Wrong input!"

convertToNumbers :: (Char, Char) -> (Int, Int)
convertToNumbers (c1, c2) = (conv1 c1, conv2 c2)
  where conv1 'A' = 1
        conv1 'B' = 2
        conv1 'C' = 3
        conv1 _ = error "Wrong input for shape1"
        conv2 'X' = 1
        conv2 'Y' = 2
        conv2 'Z' = 3
        conv2 _ = error "Wrong input for shape2"

roundP1 :: (Int, Int) -> Int
roundP1 (1, 3) = 3
roundP1 (3, 1) = 7
roundP1 (c1, c2)
  | c1 > c2  = c2
  | c1 == c2 = c2 + 3
  | c1 < c2  = c2 + 6
roundP1 (_, _) = 0

roundP2 :: (Int, Int) -> Int
roundP2 (c, 1) -- Lose
  | c == 1 = 3
  | otherwise = c - 1
roundP2 (c, 2) = c + 3
roundP2 (c, 3) -- Win
  | c == 3 = 7
  | otherwise = c + 1 + 6
roundP2 (_, _) = 0

day02 :: IO ()
day02 = do
  putStrLn "AoC 2022 day 2"
  input <- getContents
  let input_ = parseInput input
  putStrLn "Part1"
  print $ sum $ map (roundP1 . convertToNumbers) input_
  putStrLn "Part2"
  print $ sum $ map (roundP2 . convertToNumbers) input_
