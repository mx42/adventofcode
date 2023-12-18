module Day02 (day02) where

import Data.List as List
import Data.List.Split (splitOn)

import Data.Char (digitToInt, isDigit)

data Play = Play Int Int Int deriving (Show, Eq)
instance Ord Play where
  compare (Play r1 g1 b1) (Play r2 g2 b2) =
    let cr = compare r1 r2
        cg = compare g1 g2
        cb = compare b1 b2
    in case (cr, cg, cb) of
      (GT, _, _) -> GT
      (_, GT, _) -> GT
      (_, _, GT) -> GT
      _ -> LT


data Game = Game { gameNb :: Int, plays :: [Play] } deriving Show

parsePlay :: String -> Play
parsePlay input = foldl parseItem (Play 0 0 0) parts
  where parts = map ((\e -> (head e, e !! 1)) . splitOn " ") (", " `splitOn` input)
        parseItem (Play r g b) (n, "red") = Play (r + read n) g b
        parseItem (Play r g b) (n, "green") = Play r (g + read n) b
        parseItem (Play r g b) (n, "blue") = Play r g (b + read n)
        parseItem p _ = p

getGameNb :: String -> Int
getGameNb input = read $ ps !! 1
  where ps = " " `splitOn` input

parseLine :: String -> Game
parseLine input = Game gameNb plays
  where parts = ": " `splitOn` input
        gameNb = getGameNb $ head parts
        plays = map parsePlay $ "; " `splitOn` (parts !! 1)

runP1 :: Play -> Game -> Bool
runP1 limit (Game _ ps) = all (<= limit) ps

runP2 :: Game -> Int
runP2 (Game _ ps) = rs * gs * bs
  where acc = foldl accumulate (Play 0 0 0) ps
        accumulate (Play r1 g1 b1) (Play r2 g2 b2) = Play (max r1 r2) (max g1 g2) (max b1 b2)
        (Play rs gs bs) = acc

day02 :: IO ()
day02 = do
   putStrLn "AOC 2023 day 02"
   input <- getContents
   let parsed = map parseLine (lines input)
   putStrLn "Part1"
   let resP1 = filter (runP1 (Play 12 13 14)) parsed
   print $ sum $ map gameNb resP1
   putStrLn "Part2"
   let resP2 = sum $ map runP2 parsed
   print resP2
