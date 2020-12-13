module Y2020.Day13 (y20day13) where

import Data.List
import Data.List.Split
import Data.Function (on)

timeToWait :: Int -> Int -> Int
timeToWait mytime bustime = time_to_next_pass
  where time_since_last_pass = mytime `mod` bustime
        time_to_next_pass = bustime - time_since_last_pass

-- timestamp, step -> offset, busid -> timestamp, step
lowestTS :: (Int, Int) -> (Int, Int) -> (Int, Int)
lowestTS (0, 0) (_, busid) = (busid, busid)
lowestTS (curTS, step) (offset, busid)
  | (curTS + offset) `mod` busid == 0 = (curTS, step * busid)
  | otherwise = lowestTS (curTS + step, step) (offset, busid)

y20day13 :: [String] -> (String, String)
y20day13 [arrival', busids'] = (part1, part2)
  where part1 = show $ next_bus_id * next_bus_time
        part2 = show $ timestamp

        arrival = read arrival' :: Int
        (next_bus_id, next_bus_time) = minimumBy (compare `on` snd) $ map (\busid -> (busid, timeToWait arrival busid)) busids :: (Int, Int)

        busids = map read $ filter (/="x") $ splitOn "," busids' :: [Int]

        busIdWithOffset = map (\(a, b) -> (a, read b :: Int)) $ filter ((/= "x") . snd) $ zip (iterate (+1) 0) $ splitOn "," busids'
        timestamp = foldl lowestTS (0, 0) busIdWithOffset
