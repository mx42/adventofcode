module Y2020.Day14 (y20day14) where

import Data.Bits
import qualified Data.Map as M

type Bitmask = M.Map Int Bool

data Instruction = UpdateBitMask [(Int, Bool)] | SetMemory Int Int deriving (Show)

data State = State {
  bitmask :: Bitmask
  , memory :: M.Map Int Int -- [(Int, Int)]
  } deriving (Show)

parseInput :: [String] -> Instruction
parseInput ["mask", "=", mask] = UpdateBitMask $ parseBitMask 0 $ reverse mask
  where
    parseBitMask :: Int -> String -> [(Int, Bool)]
    parseBitMask _ [] = []
    parseBitMask offset (x:xs) =
      let next = (parseBitMask (offset + 1) xs) in
        case x of
          '1' -> (offset, True):next
          '0' -> (offset, False):next
          _ -> next
parseInput ["mem", offset, "=", value] = SetMemory (read offset) (read value)
parseInput s = error $ "Invalid line: " ++ (unwords s)

applyInstructionP1 :: State -> Instruction -> State
applyInstructionP1 (State _ mem) (UpdateBitMask updates) = State newMask mem
  where newMask = M.fromList updates
applyInstructionP1 (State bm mem) (SetMemory offset value) = State bm newMemory
  where newMemory = M.insert offset (maskValue value) mem
        maskValue val = checkBit val $ finiteBitSize val
          where checkBit v (-1) = v
                checkBit v offset' = checkBit newVal (offset' - 1)
                  where newVal = case offset `M.lookup` bm of
                                   Just True  -> v `setBit` offset'
                                   Just False -> v `clearBit` offset'
                                   Nothing    -> v

y20day14 :: [String] -> (String, String)
y20day14 input = (part1, part2)
  where part1 = show $ M.foldr (+) 0 $ memory endState
        part2 = show $ "WIP"
        initialState = State M.empty M.empty
        endState = foldl applyInstructionP1 initialState instructions
        instructions = map (parseInput . words . replaceBrackets) input
          where replaceBrackets ('[':xs) = ' ':(replaceBrackets xs)
                replaceBrackets (']':xs) = ' ':(replaceBrackets xs)
                replaceBrackets (x:xs) = x:(replaceBrackets xs)
                replaceBrackets [] = []
