module Day05 (day05) where
import Debug.Trace (trace)

import Data.List.Split (splitOn, chunksOf)

type Mapping = [(Int, Int, Int)]
type Mappings = [Mapping]

browseMappings :: Mappings -> Int -> Int
browseMappings [] i = i
browseMappings ([]:t) i = browseMappings t i
browseMappings (((to, from, len):t):tt) i
  | i >= from, i <= from + len =  browseMappings tt $ i - from + to
  | otherwise = browseMappings (t:tt) i

revBrowseMappings :: Mappings -> Int -> Int
revBrowseMappings [] i = i
revBrowseMappings ([]:t) i = browseMappings t i
revBrowseMappings (((from, to, len):t):tt) i
  | i >= from, i <= from + len = browseMappings tt $ i - from + to
  | otherwise = browseMappings (t:tt) i

parseMapping :: String -> Mapping
parseMapping input = numbers
  where
    input_ = tail $ lines input
    numbers = map ((\nbs -> (read $ head nbs, read $ nbs !! 1, read $ nbs !! 2)) . splitOn " ") input_

parseSeedId :: String -> [Int]
parseSeedId input = map read $ splitOn " " $ splitOn ": " input !! 1

parseInput :: String -> ([Int], Mappings)
parseInput input = (seedsId_, mappings)
  where
    seedsId_ = parseSeedId $ head split
    mappings = map parseMapping $ tail split
    split = splitOn "\n\n" input

switchSeedIdToRanges :: [Int] -> [(Int, Int)]
switchSeedIdToRanges input = map (\c -> (head c, head c + last c)) chunks
  where chunks = chunksOf 2 input

isInRanges :: [(Int, Int)] -> Int -> Bool
isInRanges [] _ = False
isInRanges ((a, b):t) v
  | v >= a && v <= b = True
  | otherwise = isInRanges t v

reverseSearchMapping :: Mappings -> [(Int, Int)] -> Int -> Int
reverseSearchMapping mappings seedRanges start
  | isInRanges seedRanges endValue = start
  | otherwise = reverseSearchMapping mappings seedRanges (start + 1)
  where endValue = revBrowseMappings mappings start

day05 :: IO ()
day05 = do
  putStrLn "AOC 2023 day 05"
  input <- getContents
  let (seeds, mappings) = parseInput input
  putStrLn "Part1"
  let resP1 = minimum $ map (browseMappings mappings) seeds
  print resP1
  putStrLn "Part2"
  putStrLn "Not working..."
  print $ mappings
  print $ reverse mappings
  let resP2 = reverseSearchMapping (reverse mappings) (switchSeedIdToRanges seeds) 0
  print resP2
