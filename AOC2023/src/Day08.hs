module Day08 (day08) where

import Debug.Trace (trace)

import qualified Data.Map.Strict as M

import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

parseLine :: String -> (String, (String, String))
parseLine input = (from, (toleft, toright))
  where from = take 3 input
        toleft = take 3 . drop 7 $ input
        toright = take 3 . drop 12 $ input

walkP1 :: M.Map String (String, String) -> String -> Int -> String -> Int
walkP1 theMap (curDir:nextDir) c curZone
  | curZone == "ZZZ" = c
  | curDir == 'L' = walkP1 theMap nextDir (c + 1) toLeft
  | curDir == 'R' = walkP1 theMap nextDir (c + 1) toRight
  where Just(toLeft, toRight) = M.lookup curZone theMap

walkP2 :: M.Map String (String, String) -> String -> Int -> [String] -> Int
walkP2 theMap (curDir:nextDir) c curZones
  | all endWithZ curZones = c
  | curDir == 'L' = walkP2 theMap nextDir (c + 1) $ map goToLeft curZones
  | curDir == 'R' = walkP2 theMap nextDir (c + 1) $ map goToRight curZones
  where endWithZ s = last s == 'Z'
        goToLeft z = fst $ fromJust $ M.lookup z theMap
        goToRight z = snd $ fromJust $ M.lookup z theMap

day08 :: IO ()
day08 = do
  putStrLn "AOC 2023 day 08"
  input <- getContents
  let inputParts = splitOn "\n\n" input
  let moveSequence = cycle $ head inputParts
  let theMap = M.fromList $ map parseLine $ lines $ last inputParts
  print $ take 10 moveSequence
  print theMap
  putStrLn "Part1"
  let resP1 = walkP1 theMap moveSequence 0 "AAA"
  print resP1
  putStrLn "Part2"
  let entranceP2 = filter (\s -> last s == 'A') $ M.keys theMap
  let resP2 = walkP2 theMap moveSequence 0 entranceP2
  print resP2
