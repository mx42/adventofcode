module Y2019.Day11 (day11) where

import qualified Data.Map as M
import Data.List
import Y2019.Intcode

getCoords :: ([Int] -> Int) -> [((Int, Int), Int)] -> (Int, Int)
getCoords fn (((x, y),_):xs) = getCoords' x y xs
  where getCoords' ax ay (((bx, by), _):xs') = getCoords' (fn [ax, bx]) (fn [ay, by]) xs'
        getCoords' ax ay [] = (ax, ay)

drawLine :: Int -> (Int, Int) -> Int -> M.Map (Int, Int) Int -> String
drawLine curY (minX, maxX) curX paint
  | curX == maxX = [color, '\n']
  | otherwise = [color] ++ drawLine curY (minX, maxX) (curX + 1) paint
  where color = if (M.findWithDefault 0 (curX, curY) paint) == 0 then '.' else '#'

draw :: M.Map (Int, Int) Int -> String
draw painting = draw' minCoords maxCoords (snd minCoords)
  where minCoords = getCoords minimum $ M.toList painting
        maxCoords = getCoords maximum $ M.toList painting
        draw' (minX, _) (maxX, maxY) curY
          | curY == maxY = drawLine curY (minX, maxX) minX painting
          | otherwise = (drawLine curY (minX, maxX) minX painting) ++
                        (draw' minCoords maxCoords (curY + 1))

day11 :: [String] -> (String, String)
day11 (input:_) = (part1, part2)
  where memory = parseProgram input
        painting = runProgramPaint memory 0
        registration = runProgramPaint memory 1
        part1 = show $ length $ painting
        part2 = draw registration
