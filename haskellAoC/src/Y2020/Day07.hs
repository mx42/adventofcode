module Y2020.Day07 (y20day07) where

import Data.List
import Data.List.Split

import Debug.Trace

type BagContents = [(Int, String)]
type Bag = (String, BagContents)
type Bags = [Bag]

cleanBag :: String -> (Int, String)
cleanBag input = (qty, name)
  where qty = read $ ((splitOn " " input) !! 0)
        name = tail $ dropWhile (/= ' ') ((splitOn " bag" input) !! 0)

parseBagContents :: String -> BagContents
parseBagContents "no other bags." = []
parseBagContents input = map cleanBag input'
  where input' = splitOn ", " input

parseLine :: String -> Bag
parseLine input = (bag_name, bag_contents)
  where input' = splitOn " contain " input
        bag_name = (splitOn " bags" (input' !! 0)) !! 0
        bag_contents = parseBagContents (input' !! 1)

getContaining :: Bags -> String -> [String]
getContaining bags name = map fst $ filter containsBag $ bags
  where containsBag bag = (>= 1) $ length $ filter (== name) $ map snd $ snd bag

getBagsContainers :: Bags -> [String] -> [String]
getBagsContainers bags input
  | input == output = input
  | otherwise = getBagsContainers bags output
  where output = nub $ input ++ concatMap (getContaining bags) input

getBagContents :: Bags -> String -> Int
getBagContents bags name = 1 + qty
  where bag = head $ filter ((== name) . fst) bags :: Bag
        qty = sum $ map countBagContents $ snd bag
        countBagContents (nb, bg) = nb * (getBagContents bags bg)

y20day07 :: [String] -> (String, String)
y20day07 input = (part1, part2)
  where part1 = show $ (+ (-1)) $ length $ getBagsContainers bags ["shiny gold"]
        part2 = show $ (+ (-1)) $ getBagContents bags "shiny gold"
        bags = map parseLine input
