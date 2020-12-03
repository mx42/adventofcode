module Y2019.Day12 (day12) where

import Data.List
import Data.List.Split

data V = V { _x :: Int, _y :: Int, _z :: Int } deriving (Eq, Show)

nullV :: V
nullV = V 0 0 0

data Moon = Moon { _pos :: V, _vel :: V } deriving (Eq, Show)

parseLine :: String -> Moon
parseLine input = Moon (V (getNb 0) (getNb 1) (getNb 2)) nullV
  where input' = filter (\c -> not $ elem c "<>,") input
        parts = splitOn " " input'
        getNb x = read $ (!! 1) $ splitOn "=" $ (parts !! x)

computeVelocity :: V -> [Moon] -> V
computeVelocity pos@(V x y z) ((Moon (V ox oy oz) _):others) = V (curX + nextX) (curY + nextY) (curZ + nextZ)
  where (V nextX nextY nextZ) = computeVelocity pos others
        curX = veloChange x ox
        curY = veloChange y oy
        curZ = veloChange z oz
        veloChange a b = if a > b
                         then -1
                         else if a < b
                              then 1
                              else 0
computeVelocity _ [] = nullV

applyVelocity :: [Moon] -> Moon -> Moon
applyVelocity others cur@(Moon curPos@(V px py pz) (V vx vy vz)) = Moon newPos newVel
  where (V nvx nvy nvz) = computeVelocity curPos others'
        others' = filter (/= cur) others
        newVel = V (vx + nvx) (vy + nvy) (vz + nvz)
        newPos = V (px + vx + nvx)  (py + vy + nvy)  (pz + vz + nvz)

computeStep :: [Moon] -> [Moon]
computeStep moons = map (applyVelocity moons) moons

moonEnergy :: Moon -> Int
moonEnergy (Moon (V px py pz) (V vx vy vz)) = potentialEnergy * kineticEnergy
  where potentialEnergy = (abs px) + (abs py) + (abs pz)
        kineticEnergy = (abs vx) + (abs vy) + (abs vz)

day12 :: [String] -> (String, String)
day12 input = (part1, part2)
  where planets = map parseLine $ filter (not . null) input
        iteration = iterate computeStep planets

        at1000 = iteration !! 1000
        part1 = show $ sum $ map moonEnergy $ at1000

        -- ok, inefficient, need something better
        idx = elemIndex planets $ tail iteration
        part2 = show $ fmap (+1) idx
