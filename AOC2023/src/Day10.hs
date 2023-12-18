module Day10 (day10) where

import Debug.Trace (trace)


pos2xy :: Int -> Int -> (Int, Int)
pos2xy width pos = (curX, curY)
  where curX   = mod pos width
        curY   = pos `div` width

xy2pos :: Int -> (Int, Int) -> Int
xy2pos width (x, y) = x + width * y

getNeighbors :: (Int, Int) -> Int -> [Int]
getNeighbors (w, h) p = map (xy2pos w) $ filter (\(x', y') -> x' >= 0 && x' < (w - 1) && y' >= 0 && y' < h) candidates
  where candidates = [(x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1)]
        p' = pos2xy w p
        x = fst p'
        y = snd p'

getStartPos :: String -> Int
getStartPos input = head sPos
  where charPos = zip input [0..]
        sPos = map snd $ filter (\(c, _) -> c == 'S') charPos

computeDiff :: (Int, Int) -> (Int, Int) -> (Int, Int)
computeDiff (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

opening :: Char -> (Int, Int) -> Bool
opening 'S' _ = True
opening 'F' (1, 0) = True
opening 'F' (0, 1) = True
opening 'L' (1, 0) = True
opening 'L' (0, -1) = True
opening 'J' (-1, 0) = True
opening 'J' (0, -1) = True
opening '7' (-1, 0) = True
opening '7' (0, 1) = True
opening '-' (_, 0) = True
opening '|' (0, _) = True
opening _ _ = False

walkable :: String -> Int -> Int -> Bool
walkable mapData curPos destPos
  | destChar == '.' = False
  | otherwise = canLeaveCur && canArriveTo
  where lined       = lines mapData
        width       = 1 + length (head lined)
        curChar     = mapData !! curPos
        destChar    = mapData !! destPos
        dirFrom     = computeDiff (pos2xy width curPos) (pos2xy width destPos)
        dirTo       = computeDiff (pos2xy width destPos) (pos2xy width curPos)
        canLeaveCur = opening curChar dirFrom
        canArriveTo = opening destChar dirTo

getWalkableNeighbors :: String -> Int -> [Int]
getWalkableNeighbors mapData pos = filter (walkable mapData pos) $ getNeighbors (mapWidth, mapHeight) pos
  where lined     = lines mapData
        mapWidth  = 1 + length (head lined)
        mapHeight = length lined

walkNode :: String -> ([(Int, Int)], [(Int, Int)]) -> ([(Int, Int)], [(Int, Int)])
walkNode _ t@([], _) = t
walkNode mapData (cur@(pos,distance):t, walked) = walkNode mapData (t ++ filteredWalkableNeighbors, cur:walked)
  where walkableNeighbors = getWalkableNeighbors mapData pos
        filteredWalkableNeighbors = map (\p -> (p, distance + 1)) $ filter (\p -> p `notElem` map fst (t ++ walked)) walkableNeighbors

cleanInput :: [Int] -> [(Int, Char)] -> String
cleanInput _ [] = []
cleanInput ps ((_, '\n'):t) = '\n':cleanInput ps t
cleanInput ps ((p, c):t)
  | p `elem` ps = c:cleanInput ps t
  | otherwise = ' ':cleanInput ps t

day10 :: IO ()
day10 = do
  putStrLn "AOC 2023 day 10"
  input <- getContents
  putStrLn "Part1"
  mapM_ print $ lines input
  let startPos = getStartPos input
  let (_, computedLoop) = walkNode input ([(startPos, 0)], [])
  print $ snd $ head computedLoop
  putStrLn "Part2"
  let loopPos = map fst computedLoop
  let cleaned = cleanInput loopPos (zip [0..] input)
  mapM_ print $ lines cleaned