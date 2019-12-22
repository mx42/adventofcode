module Day10 (day10) where

import Data.List
import Geo.Vector
import Geo.Point

parseLine :: Int -> [Char] -> [Int]
parseLine xp ('#':cs) = xp:(parseLine (xp + 1) cs)
parseLine xp (_:cs) = parseLine (xp + 1) cs
parseLine _ [] = []

parseInput :: Int -> [String] -> [Point]
parseInput yp (l:ls) = (map convertToPoint $ parseLine 0 l) ++ parseInput (yp + 1) ls
  where convertToPoint xp = Point (fromIntegral xp) (fromIntegral yp)
parseInput _ [] = []

getDirVectors :: Point -> [Point] -> [Vector]
getDirVectors src (dest:dests) = computedVect:(getDirVectors src dests)
  where computedVect = roundVecAtN 5 $ directionalVectorFromPoint (dest `sub` src)
getDirVectors _ [] = []

getVectorAndDistPerPoint :: [Point] -> Point -> [(Point, Double, Double)]
getVectorAndDistPerPoint (dest:dests) src = (dest, computedVect, distance):nextEntries
  where computedVect = vec2angle $ directionalVectorFromPoint diffPt
        distance = roundN 5 $ sqrt $ ((Geo.Point.x diffPt) ^ 2) + ((Geo.Point.y diffPt) ^ 2)
        diffPt = dest `sub` src
        nextEntries = getVectorAndDistPerPoint dests src
getVectorAndDistPerPoint [] _ = []

asteroidSorting :: (Point, Double, Double) -> (Point, Double, Double) -> Ordering
asteroidSorting (_, angle1, dist1) (_, angle2, dist2)
  | angle1 > angle2 = GT
  | angle1 < angle2 = LT
  | dist1 > dist2 = GT
  | dist1 < dist2 = LT
  | otherwise = EQ

getLaserOrder :: [[Point]] -> [Point]
getLaserOrder ((p:[]):xs) = p:(getLaserOrder xs)
getLaserOrder ((p:ps):xs) = p:(getLaserOrder (xs ++ [ps]))
getLaserOrder [] = []

day10 :: IO ()
day10 = do
  putStrLn $ "AoC 2019 day 10"
  input <- getContents
  let asteroids = parseInput 0 $ lines input

  let part1 = maximumBy (\(_, a) (_, b) -> compare a b) $ map countFn asteroids
        where countFn pt = (pt, (length $ nub $ getDirVectors pt asteroids) - 1)

  putStrLn $ "Part 1: " ++ (show $ part1)

  let p2 = getLaserOrder $
        map (\m -> map (\(p, _, _) -> p) m) $
        groupBy (\(_, v1, _) (_, v2, _) -> v1 == v2) $
        sortBy asteroidSorting $
        getVectorAndDistPerPoint asteroids $
        fst part1

  putStrLn $ "Part 2: " ++ (show $ p2 !! 200)
