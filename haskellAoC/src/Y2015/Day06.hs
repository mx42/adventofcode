module Y2015.Day06 (y15day06) where

import Data.List.Split

data Ranges = Ranges {
  minWidth :: Int
  , maxWidth :: Int
  , minHeight :: Int
  , maxHeight :: Int
  } deriving Show

data Instruction = Toggle Ranges | TurnOn Ranges | TurnOff Ranges deriving Show

type Lights = [Bool]

getRanges :: [String] -> Ranges
getRanges parts = Ranges minW maxW minH maxH
  where start = map read $ splitOn "," $ parts !! 0 :: [Int]
        end = map read $ splitOn "," $ parts !! 2 :: [Int]
        widths = [start !! 0, end !! 0]
        heights = [start !! 1, end !! 1]
        minW = minimum widths
        maxW = maximum widths
        minH = minimum heights
        maxH = maximum heights

parseEntry :: [String] -> Instruction
parseEntry parts
  | parts !! 0 == "turn" && parts !! 1 == "on" = TurnOn $ getRanges $ tail $ tail $ parts
  | parts !! 0 == "turn" && parts !! 1 == "off" = TurnOff $ getRanges $ tail $ tail $ parts
  | parts !! 0 == "toggle" = Toggle $ getRanges $ tail $ parts

applyInstructionP1 :: (Int, Int) -> Bool -> Instruction -> Bool
applyInstructionP1 (x, y) cur (TurnOn (Ranges minX maxX minY maxY))
  | x >= minX && x <= maxX && y >= minY && y <= maxY = True
  | otherwise = cur
applyInstructionP1 (x, y) cur (TurnOff (Ranges minX maxX minY maxY))
  | x >= minX && x <= maxX && y >= minY && y <= maxY = False
  | otherwise = cur
applyInstructionP1 (x, y) cur (Toggle (Ranges minX maxX minY maxY))
  | x >= minX && x <= maxX && y >= minY && y <= maxY = not cur
  | otherwise = cur

applyInstructionsP1 :: [Instruction] -> Bool -> (Int, Int) -> Bool
applyInstructionsP1 [] cur _ = cur
applyInstructionsP1 (inst:next) cur pos = applyInstructionsP1 next newState pos
  where newState = applyInstructionP1 pos cur inst

countLightsP1 :: [Instruction] -> Int
countLightsP1 instr = length $ filter (== True) $ map (applyInstructionsP1 instr False) allPos
  where allPos = map (\n -> (n `div` 1000, mod n 1000)) $ take 1000000 $ iterate (+1) 0

applyInstructionP2 :: (Int, Int) -> Int -> Instruction -> Int
applyInstructionP2 (x, y) cur (TurnOn (Ranges minX maxX minY maxY))
  | x >= minX && x <= maxX && y >= minY && y <= maxY = cur + 1
  | otherwise = cur
applyInstructionP2 (x, y) cur (TurnOff (Ranges minX maxX minY maxY))
  | x >= minX && x <= maxX && y >= minY && y <= maxY && cur > 0 = cur - 1
  | otherwise = cur
applyInstructionP2 (x, y) cur (Toggle (Ranges minX maxX minY maxY))
  | x >= minX && x <= maxX && y >= minY && y <= maxY = cur + 2
  | otherwise = cur

applyInstructionsP2 :: [Instruction] -> Int -> (Int, Int) -> Int
applyInstructionsP2 [] cur _ = cur
applyInstructionsP2 (inst:next) cur pos = applyInstructionsP2 next newState pos
  where newState = applyInstructionP2 pos cur inst

countLightsP2 :: [Instruction] -> Int
countLightsP2 instr = sum $ map (applyInstructionsP2 instr 0) allPos
  where allPos = map (\n -> (n `div` 1000, mod n 1000)) $ take 1000000 $ iterate (+1) 0


y15day06 :: [String] -> (String, String)
y15day06 input = (part1, part2)
  where part1 = show $ countLightsP1 instructions
        part2 = show $ countLightsP2 instructions
        instructions = map (parseEntry . splitOn " ") input
