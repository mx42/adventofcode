{-# LANGUAGE TupleSections #-}
module Day11 (day11) where
import Data.List (elemIndices)

getRowsToExpand :: [String] -> [Int]
getRowsToExpand input = filter (\i -> all (== '.') (input !! i)) [0..(length input - 1)]

getColsToExpand :: [String] -> [Int]
getColsToExpand input = filter (all (== '.') . byCol) [0..(length (head input) - 1)]
  where byCol n = map (!! n) input

getGalaxiesPos :: [String] -> [(Int, Int)]
getGalaxiesPos input = concatMap getGalaxiesAtLine [0..(length input - 1)]
  where getGalaxiesAtLine n = map (, n) (elemIndices '#' (input !! n))

getPairs :: [a] -> [(a, a)]
getPairs [] = []
getPairs (h:t) = map (h,) t ++ getPairs t

getDistance :: [Int] -> [Int] -> Int -> (Int, Int) -> (Int, Int) -> Int
getDistance cols rows factor (x1, y1) (x2, y2) = abs (y2 - y1) + abs (x2 - x1) + ((rowsExpansion + colsExpansion) * factor)
  where rowsExpansion = length $ filter (\i -> i > minimum [y1, y2] && i < maximum [y1, y2]) rows
        colsExpansion = length $ filter (\i -> i > minimum [x1, x2] && i < maximum [x1, x2]) cols

day11 :: IO ()
day11 = do
  putStrLn "AOC 2023 day 11"
  input <- getContents
  let input' = lines input
  let colsToExpand = getColsToExpand input'
  let rowsToExpand = getRowsToExpand input'
  let galaxiesPos = getGalaxiesPos input'
  let pairs = getPairs galaxiesPos
  putStrLn "Part1"
  print $ sum $ map (uncurry (getDistance colsToExpand rowsToExpand 1)) pairs
  putStrLn "Part2"
  print $ sum $ map (uncurry (getDistance colsToExpand rowsToExpand (1000000 - 1))) pairs
