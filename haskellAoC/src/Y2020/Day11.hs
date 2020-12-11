module Y2020.Day11 (y20day11) where

import Data.Matrix

type Setting = Matrix Char

empty :: Char
empty = 'L'

occupied :: Char
occupied = '#'

directions :: [(Int, Int)]
directions = [(-1, -1), (0, -1), (1, -1),
              (-1,  0),          (1,  0),
              (-1,  1), (0,  1), (1,  1)]

countOccupiedSeatsP1 :: Setting -> (Int, Int) -> Int
countOccupiedSeatsP1 old (x, y) = length $ filter (== occupied) $ map ((!) old) $ filter validPos $ adjacentSeats
  where adjacentSeats = map (\(dx, dy) -> (x + dx, y + dy)) $ directions
        validPos (x', y') = x' >= 1 && y' >= 1 && x' <= (ncols old) && y' <= (nrows old)

countOccupiedSeatsP2 :: Setting -> (Int, Int) -> Int
countOccupiedSeatsP2 old p = length $ filter (== occupied) $ map (getFirstSeat p) $ directions
  where validPos (x, y) = x >= 1 && y >= 1 && x <= (ncols old) && y <= (nrows old)
        getFirstSeat (x, y) (dx, dy)
          | not $ validPos (x', y') = '.'
          | inSight == '.' = getFirstSeat (x', y') (dx, dy)
          | otherwise = inSight
          where x' = x + dx
                y' = y + dy
                inSight = old ! (x', y')

generatorFunc :: Int -> Setting -> (Int, Int) -> Char
generatorFunc part old p
  | oldPos == empty && (occupiedSeats part) == 0 = occupied
  | oldPos == occupied && (occupiedSeats part) >= (occupiedThreshold part) = empty
  | otherwise = oldPos
  where oldPos = old ! p
        occupiedSeats 1 = countOccupiedSeatsP1 old p
        occupiedSeats 2 = countOccupiedSeatsP2 old p
        occupiedThreshold 1 = 4
        occupiedThreshold 2 = 5

nextSetting :: Int -> Setting -> Setting
nextSetting part setting = matrix cols rows $ generatorFunc part setting
  where cols = ncols setting
        rows = nrows setting

hasChanged :: Setting -> Setting -> Bool
hasChanged previous current = previous /= current

getStabilizedSetting :: Int -> Setting -> Setting
getStabilizedSetting part setting
  | nextHasChanged == True = getStabilizedSetting part next
  | otherwise = setting
  where next = nextSetting part setting
        nextHasChanged = hasChanged setting next

y20day11 :: [String] -> (String, String)
y20day11 input = (part1, part2)
  where part1 = show $ countOccupied $ getStabilizedSetting 1 input'
        part2 = show $ countOccupied $ getStabilizedSetting 2 input'
        input' = fromLists input
        countOccupied mat = length $ filter (== occupied) $ toList mat
