module Y2015.Day09 (y15day09) where

import qualified Data.Map as M

import Data.List
import Data.List.Split

parseInput :: String -> (String, String, Int)
parseInput input = parseInput' $ splitOn " " input
  where parseInput' [c1, "to", c2, "=", d] = (c1, c2, read d)

getCities :: [(String, String, Int)] -> [String]
getCities input = nub $ cities
  where cities = getCities' input
        getCities' [] = []
        getCities' ((c1, c2, _):next) = c1:c2:(getCities' next)

distanceToTuples :: (String, String, Int) -> [((String, String), Int)]
distanceToTuples (c1, c2, d) = [((c1, c2), d), ((c2, c1), d)]

computeTotalDistance :: M.Map (String, String) Int -> [String] -> Int
computeTotalDistance dists (c1:c2:next) = (dists M.! (c1, c2)) + computeTotalDistance dists (c2:next)
computeTotalDistance _ _ = 0

-- A bit naive implementation, but the low volume of cities (permutations) make it doable

y15day09 :: [String] -> (String, String)
y15day09 input = (part1, part2)
  where part1 = show $ minimum $ allDistances
        part2 = show $ maximum $ allDistances
        distances = map parseInput input
        cities = getCities distances
        distancesMap = M.fromList $ concatMap distanceToTuples distances
        allPermutations = permutations $ cities
        allDistances = map (computeTotalDistance distancesMap) $ allPermutations
