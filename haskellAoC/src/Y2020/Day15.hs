module Y2020.Day15 (y20day15) where

import qualified Data.IntMap as IM
import           Data.List.Split

data State = State {
  turn :: Int
  , prevOffset :: IM.IntMap Int
  , memory :: [Int]
  } deriving (Show)

playGame :: State -> State
playGame (State t prev xs@(x:_)) =
  let newOffsets = IM.insert x (t - 1) prev
      newTurn = (t + 1)
      newState mem = State newTurn newOffsets mem
  in
    case x `IM.lookup` prev of
      Nothing -> newState (0:xs)
      Just n  -> newState (y:xs)
        where y = (t - 1) - n

playNtimes :: Int -> State -> State
playNtimes n input
  | (turn input) == n = input
  | otherwise = playNtimes n $ playGame input

memToOffsets :: Int -> [Int] -> [(Int, Int)]
memToOffsets _ [] = []
memToOffsets ofs (h:t) = (h,ofs):(memToOffsets (ofs+1) t)

y20day15 :: [String] -> (String, String)
y20day15 (input:_) = (part1, part2)
  where part1 = show $ head $ memory $ playNtimes (2020 + 1) initialState
        part2 = show $ head $ memory $ playNtimes (30000000 + 1) initialState

        initialState = State initialTurn initialPrevious initialMemory

        initialTurn = (1 + length initialMemory)
        initialPrevious = (IM.fromList $ memToOffsets 1 $ reverse $ tail $ initialMemory)
        initialMemory = reverse $ map read $ splitOn "," input :: [Int]
