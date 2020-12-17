module Y2015.Day14 (y15day14) where

import Data.Function (on)
import Data.List

data Reindeer = Reindeer {
  name           :: String
  , speed        :: Int
  , flyDuration  :: Int
  , restDuration :: Int
  } deriving (Show)

time :: Int
time = 2503

parseLine :: String -> Reindeer
parseLine input = Reindeer n s fd rd
  where input' = words input
        n      = input' !! 0
        s      = read $ input' !! 3
        fd     = read $ input' !! 6
        rd     = read $ input' !! 13

reindeerDistanceAt :: Int -> Reindeer -> Int
reindeerDistanceAt ts (Reindeer _ s d r) = comp * d * s + (min remn d) * s
  where cycleTime    = d + r
        (comp, remn) = ts `quotRem` cycleTime

reindeersInFrontAt :: [Reindeer] -> Int -> [String]
reindeersInFrontAt rs t = map (name . fst) $ filter ((== leadDistance) . snd) rsWithDist
  where leadDistance    = snd $ maximumBy (compare `on` snd) $ rsWithDist
        rsWithDist      = map (\r -> (r, reindeerDistanceAt t r)) rs

getMaxPoints :: Int -> [Reindeer] -> Int
getMaxPoints duration rs = maximum $ map length $ group $ sort $
                           concatMap (reindeersInFrontAt rs) $ [1..duration]

y15day14 :: [String] -> (String, String)
y15day14 input = (part1, part2)
  where part1  = show $ maximum $ map (reindeerDistanceAt time) $ reindeers
        part2  = show $ getMaxPoints time reindeers

        reindeers = map parseLine input
