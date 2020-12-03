module Y2019.Day08 (day8) where

import Data.List.Split
import Data.List

width :: Int
width = 25

height :: Int
height = 6

buildLayers :: String -> Int -> Int -> [[Char]]
buildLayers "" _ _ = []
buildLayers input w h = front:(buildLayers back w h)
  where (front, back) = splitAt (w * h) input

decodeLayers :: [String] -> String
decodeLayers layers = foldl decodeLayers' (replicate (width * height) '2') layers
  where decodeLayers' acc layer = map (\(a, l) -> if a == '2' then l else a) (zip acc layer)

day8 :: [String] -> (String, String)
day8 (input:_) = (part1, part2)
  where layers = buildLayers input width height
        countDigit d = length . (filter (== d))
        min0layer = minimumBy (\ a b -> (countDigit '0' a) `compare` (countDigit '0' b)) layers
        p1nbOf1 = countDigit '1' min0layer
        p1nbOf2 = countDigit '2' min0layer

        part1 = show $ p1nbOf1 * p1nbOf2

        decoded = unlines (chunksOf width (decodeLayers layers))

        repl '0' = ' '
        repl '1' = '#'
        repl c   = c

        part2 = map repl decoded
