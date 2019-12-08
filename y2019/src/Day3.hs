module Day3 (day3) where

import           Data.List.Split
import           Data.List

import qualified Data.Map as Map

import           Geo.Point
import           Geo.Segment
import           Geo.Vector

type Coords = Map.Map Int [(Int, Int)]

parseElem :: [Char] -> Vector
parseElem ('U':count) = let n = read count in Vector n 0
parseElem ('D':count) = let n = read count in Vector (-n) 0
parseElem ('L':count) = let n = read count in Vector 0 (-n)
parseElem ('R':count) = let n = read count in Vector 0 n
parseElem _ = nilVector

seg2coords :: (Coords, Coords) -> Segment -> (Coords, Coords)
seg2coords (accH, accV) (Segment (Point x1 y1) (Point x2 y2))
  | x1' == x2' = (accH, Map.insertWith (++) x1' [(y1', y2')] accV)
  | y1' == y2' = ((Map.insertWith (++) y1' [(x1', x2')] accH), accV)
  | otherwise = (accH, accV)
  where x1' = round x1
        x2' = round x2
        y1' = round y1
        y2' = round y2

computeIntersections' :: Coords -> [Point] -> Int -> [(Int, Int)] -> [Point]
computeIntersections' hcoords ps vx vys = points ++ ps
  where hinRange = Map.filterWithKey (\hy hxs -> (any (inRange hy) vys) && (any (inRange vx) hxs)) hcoords
        inRange p (p1, p2) = if p1 < p2 then p1 <= p && p <= p2 else p2 <= p && p <= p1
        points = map (\hy -> Point (fromIntegral vx) (fromIntegral hy)) (Map.keys hinRange)

computeIntersections :: (Coords, Coords) -> (Coords, Coords) -> [Point]
computeIntersections (vertShape1, horizShape1) (vertShape2, horizShape2) = intersections1 ++ intersections2
  where compute vs hs = Map.foldlWithKey (computeIntersections' hs) [] vs
        intersections1 = compute vertShape1 horizShape2
        intersections2 = compute horizShape1 vertShape2

parseSegments :: String -> [Segment]
parseSegments input = reverse (linkVectors (map parseElem (splitOn "," input)))

getCoordsFromSegs :: [Segment] -> (Coords, Coords)
getCoordsFromSegs segments = foldl seg2coords (Map.empty, Map.empty) segments

getDistanceOnShape :: [Segment] -> Point -> Int
getDistanceOnShape (seg@(Segment s d):xs) p
  |p `onSegment` seg = round (manhattanDist s p)
  |otherwise = round (manhattanDist s d) + (getDistanceOnShape xs p)
getDistanceOnShape [] _ = 0

getIntersectionDist :: [Segment] -> [Segment] -> Point -> Int
getIntersectionDist s1 s2 p = getDistanceOnShape s1 p + getDistanceOnShape s2 p

day3 :: IO ()
day3 = do
  putStrLn "AoC 2019 day 3"
  putStr "Enter input >"

  firstInput <- getLine
  secondInput <- getLine
  putStrLn ""

  let firstShape = parseSegments firstInput
  let firstCoords = getCoordsFromSegs firstShape
  let secondShape = parseSegments secondInput
  let secondCoords = getCoordsFromSegs secondShape

  let intersections = computeIntersections firstCoords secondCoords

  let part1 = manhattanDist origin (head (sortOn (manhattanDist origin) intersections))
  putStr "Part1: "
  print part1
  putStrLn ""

  let intersectionDists = map (getIntersectionDist firstShape secondShape) intersections
  let part2 = minimum intersectionDists

  putStr "Part2: "
  print part2
  putStrLn ""
