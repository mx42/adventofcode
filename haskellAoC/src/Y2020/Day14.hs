module Y2020.Day14 (y20day14) where

import Data.Bits
import qualified Data.Map as M

-- map offset -> 1 (just true), 0 (just false), X (nothing)
-- list of combinations values for the Xs
type Bitmask = (M.Map Int (Maybe Bool), [Int])

data Instruction = UpdateBitMask [(Int, Maybe Bool)] | SetMemory Int Int deriving (Show)

data State = State {
  bitmask :: Bitmask
  , memory :: M.Map Int Int
  } deriving (Show)

parseBitMask :: Int -> String -> [(Int, Maybe Bool)]
parseBitMask _ [] = []
parseBitMask offset (x:xs) =
  let next = (parseBitMask (offset + 1) xs) in
    case x of
      '1' -> (offset, Just True):next
      '0' -> (offset, Just False):next
      _ -> (offset, Nothing):next

parseInput :: [String] -> Instruction
parseInput ["mask", "=", mask] = UpdateBitMask $ parseBitMask 0 $ reverse mask
  where
parseInput ["mem", offset, "=", value] = SetMemory (read offset) (read value)
parseInput s = error $ "Invalid line: " ++ (unwords s)

applyInstructionP1 :: State -> Instruction -> State
applyInstructionP1 (State _ mem) (UpdateBitMask updates) = State newMask mem
  where newMask = (M.fromList updates, [])
applyInstructionP1 (State (bm, _) mem) (SetMemory offset value) = State (bm, []) newMemory
  where newMemory = M.insert offset (maskValue value) mem
        maskValue val = checkBit val $ finiteBitSize val
          where checkBit v (-1) = v
                checkBit v offset' = checkBit newVal (offset' - 1)
                  where newVal = case offset `M.lookup` bm of
                                   Just (Just True)  -> v `setBit` offset'
                                   Just (Just False) -> v `clearBit` offset'
                                   _                 -> v

-- arg1: available combinations
-- arg2: variable offsets
-- arg3: final combinations
computeBitCombinations :: [Int] -> [Int] -> [Int]
computeBitCombinations [] (b:bs) = computeBitCombinations [0, 0 `setBit` b] bs
computeBitCombinations done [] = done
computeBitCombinations done (b:bs) = computeBitCombinations (concatMap (\x -> [x, x `setBit` b]) done) bs

-- p2 :
-- compute value (of address) with mask considering Xs => &0, other offsets: |
-- compute all possible values of Xs (X00X -> 0000, 0001, 1000, 1001)
-- for each possible values of Xs, add initial masked value

-- mask:            X1001X
-- with address 42: 101010

-- masked address : 011010 (26)
-- 4 possibilities: 000000 (0)  + 26 = 26
--                  000001 (1)  + 27 = 27
--                  100000 (32) + 32 = 58
--                  100001 (33) + 33 = 59

-- mask:            00X0XX
-- with address 26: 011010
-- masked address:  010000 (16)

-- 8 possibilities: 000000 (0)  + 16 = 16
--                  000001 (1)  + 16 = 17
--                  000010 (2)  + 16 = 18
--                  000011 (3)  + 16 = 19
--                  001000 (8)  + 16 = 24
--                  001001 (9)  + 16 = 25
--                  001010 (10) + 16 = 26
--                  001011 (11) + 16 = 27

applyInstructionP2 :: State -> Instruction -> State
applyInstructionP2 (State _ mem) (UpdateBitMask updates) = State newMask mem
  where newMask = (bitMask, variableCombinations)
        variableCombinations = computeBitCombinations [] $ map fst $ filter ((== Nothing) . snd) $ updates
        bitMask = M.fromList $ filter ((/= Just False) . snd) $ updates
applyInstructionP2 (State (bm, var) mem) (SetMemory offset value) = newState
  where newState  = State (bm, var) newMemory
        newMemory = foldr (\k -> M.insert k value) mem addresses
        addresses = map (\v -> v + offset') var
        offset'   = checkBit offset $ finiteBitSize offset
          where checkBit v (-1) = v
                checkBit v o = checkBit newVal (o - 1)
                  where newVal = case o `M.lookup` bm of
                                   Just (Just True)  -> v `setBit` o
                                   Just (Nothing)    -> v `clearBit` o
                                   _                 -> v


y20day14 :: [String] -> (String, String)
y20day14 input = (part1, part2)
  where part1 = show $ M.foldr (+) 0 $ memory endStateP1
        part2 = show $ M.foldr (+) 0 $ memory endStateP2
        initialState = State (M.empty, []) M.empty
        endStateP1 = foldl applyInstructionP1 initialState instructions
        endStateP2 = foldl applyInstructionP2 initialState instructions
        instructions = map (parseInput . words . replaceBrackets) input
          where replaceBrackets ('[':xs) = ' ':(replaceBrackets xs)
                replaceBrackets (']':xs) = ' ':(replaceBrackets xs)
                replaceBrackets (x:xs) = x:(replaceBrackets xs)
                replaceBrackets [] = []
