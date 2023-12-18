module Day03 (day03) where

import Data.Char (isDigit, digitToInt)

import qualified Data.Map as Map

data Pos = Pos Int Int deriving (Show, Ord, Eq)

data Symbol = Symbol { symbolChar :: Char, symbolPos :: Pos } deriving (Show, Eq)

data Part = Part { pos :: Pos, partNb :: Int, partSymbols :: [Symbol] } deriving (Show)

isNeighbor :: Pos -> Pos -> Bool
isNeighbor (Pos x1 y1) (Pos x2 y2) = (abs (x2 - x1) <= 1) && (abs (y2 - y1) <= 1)

getNeighborSymbols :: [Symbol] -> Pos -> [Symbol]
getNeighborSymbols symbols p = filter (\(Symbol _ ps) -> isNeighbor p ps) symbols

getAccNumbers :: [Symbol] -> [Part] -> (Pos, Int) -> [Part]
getAccNumbers allSymbols [] (curPos, curNb) = [Part curPos curNb neighborSymbols]
    where neighborSymbols = getNeighborSymbols allSymbols curPos
getAccNumbers allSymbols (curPart@(Part (Pos accX accY) accInt symbols):t) (curPos@(Pos curX curY), curInt)
  | accX == curX && accY == curY - 1 = Part curPos (accInt * 10 + curInt) (neighborSymbols ++ symbols) : t
  | otherwise = Part curPos curInt neighborSymbols : curPart : t
    where neighborSymbols = getNeighborSymbols allSymbols curPos

getParts :: Map.Map Pos Int -> [Symbol] -> [Part]
getParts numbers symbols = foldl accNumber [] (Map.toList numbers)
  where accNumber = getAccNumbers symbols


parseInput :: String -> ([Part], [Symbol])
parseInput input = (parts, symbols)
  where chars = [ (Pos x y, c) | (x, row) <- zip [0..] (lines input), (y, c) <- zip [0..] row, c /= '.' ]
        numbers = Map.fromList $ map (\(k, v) -> (k, digitToInt v)) $ filter (\(_, v) -> isDigit v) chars :: Map.Map Pos Int
        symbols = map (\(k, v) -> Symbol v k) $ filter (\(_, v) -> not $ isDigit v) chars  :: [Symbol]
        parts = getParts numbers symbols


day03 :: IO ()
day03 = do
   putStrLn "AOC 2023 day 03"
   input <- getContents
   let (parts, symbols) = parseInput input
   putStrLn "Part1"
   let part1 = sum $ map partNb $ filter (not . null . partSymbols) parts
   print part1
   putStrLn "Part2"
   let allStars = filter ((== '*') . symbolChar) symbols
   let part2 = sum $ map (\ps -> head ps * ps !! 1) $ filter ((== 2) . length) $ map (\s -> map partNb $ filter (elem s . partSymbols) parts) allStars
   print part2
