module Day8 (day8) where

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

day8 :: IO ()
day8 = do
  putStrLn $ "AoC 2019 day 8"
  input <- getLine

  let layers = buildLayers input width height

  let countDigit d = length . (filter (== d))

  let min0layer = minimumBy (\ a b -> (countDigit '0' a) `compare` (countDigit '0' b)) layers

  let p1nbOf1 = countDigit '1' min0layer
  let p1nbOf2 = countDigit '2' min0layer

  putStrLn $ "Part 1: " ++ (show (p1nbOf1 * p1nbOf2))

  putStrLn "Part 2:"

  let decoded = unlines (chunksOf width (decodeLayers layers))

  let repl '0' = ' '
      repl '1' = '#'
      repl c   = c
    in putStrLn $ map repl decoded
