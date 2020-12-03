module Y2019.Day06 (day6) where

import           Data.Maybe
import qualified Data.Map as Map
import           Data.List.Split
import qualified Data.Set as Set

-- https://stackoverflow.com/questions/22403029/how-to-zip-lists-with-different-length
zipWithPadding :: a -> b -> [a] -> [b] -> [(a,b)]
zipWithPadding a b (x:xs) (y:ys) = (x,y) : zipWithPadding a b xs ys
zipWithPadding a _ []     ys     = zip (repeat a) ys
zipWithPadding _ b xs     []     = zip xs (repeat b)

orbitLength :: Map.Map String String -> String -> Int
orbitLength orbits object
  | isJust orbitOn = 1 + (orbitLength orbits (head (maybeToList orbitOn)))
  | isNothing orbitOn = 0
  where orbitOn = object `Map.lookup` orbits

getOrbits :: Map.Map String String -> String -> [String]
getOrbits orbits object
  | isJust orbitOn = object:(getOrbits orbits (head (maybeToList orbitOn)))
  | isNothing orbitOn = object:[]
  where orbitOn = object `Map.lookup` orbits

countTuples :: [(String, String)] -> Int
countTuples (("", _):xs) = 1 + countTuples xs
countTuples ((_, ""):xs) = 1 + countTuples xs
countTuples ((_, _):xs) = 2 + countTuples xs
countTuples [] = 0

day6 :: [String] -> (String, String)
day6 input = (part1, part2)
  where orbitsMap = Map.fromList (map (\r -> (r !! 1, r !! 0)) (map (splitOn ")") input))
        orbitingObjects = Map.keysSet orbitsMap
        nbOrbitsPerObject = map (orbitLength orbitsMap) (Set.toList orbitingObjects)
        part1 = show $ sum nbOrbitsPerObject
        myOrbits = reverse $ tail $ getOrbits orbitsMap "YOU"
        santaOrbits = reverse $ tail $ getOrbits orbitsMap "SAN"
        elems = dropWhile (\(a, b) -> a == b) (zipWithPadding [] [] myOrbits santaOrbits)
        part2 = show $ countTuples elems
