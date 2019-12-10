module Intcode (parseProgram, runProgramV1, runProgramV2) where

import Data.Map
import Data.List.Split
import Data.Char

import Debug.Trace
-- trace _ x = x

data ParamMode = Position | Direct | Relative deriving (Show)

data State = State { _index :: Int,
                     _relIndex :: Int,
                     _input :: [Int],
                     _output :: [Int],
                     _memory :: Map Int Int,
                     _running :: Bool } deriving (Show)

data Instr = Instr { _opcode :: Int,
                     _p1 :: (ParamMode, Int),
                     _p2 :: (ParamMode, Int),
                     _p3 :: (ParamMode, Int)
                   }

-- computeOpCode: Take an INT and returns a tuple4:
-- - Int = opcode
-- - Subsequent ints = Mode of nth param (0 = pos, 1 = direct, 2 = relative)
computeOpCode :: Int -> (Int, ParamMode, ParamMode, ParamMode)
computeOpCode input = result -- trace ("Compute opcode " ++ show input ++ " -> " ++ show result) result
  where opcode = input `mod` 100
        digits = show input
        len = length digits
        mode1 = convertToPM $ if len > 2 then (ord $ digits !! (len - 3)) - 48 else 0
        mode2 = convertToPM $ if len > 3 then (ord $ digits !! (len - 4)) - 48 else 0
        mode3 = convertToPM $ if len > 4 then (ord $ digits !! (len - 5)) - 48 else 0
        result = (opcode, mode1, mode2, mode3)
        convertToPM 0 = Position
        convertToPM 1 = Direct
        convertToPM 2 = Relative

getInstruction :: State -> Instr
getInstruction s = Instr opcode param1 param2 param3
  where (opcode, m1, m2, m3) = computeOpCode $ getMemoryAt 0
        param1 = (m1, getMemoryAt 1)
        param2 = (m2, getMemoryAt 2)
        param3 = (m3, getMemoryAt 3)
        getMemoryAt n = (_memory s) ! (n + _index s)

getValueSafe :: State -> (ParamMode, Int) -> Int
getValueSafe _ (Position, n) = n
getValueSafe _ (Direct, n) = n
getValueSafe s (Relative, n) = findWithDefault 0 (n + _relIndex s) (_memory s)

getValue :: State -> (ParamMode, Int) -> Int
getValue s (Position, n) = findWithDefault 0 n (_memory s)
getValue _ (Direct, n) = n
getValue s (Relative, n) = findWithDefault 0 (n + _relIndex s) (_memory s)

compute :: State -> Instr -> State
-- OPCODE 1 - ADDITION
compute s (Instr 1 p1 p2 p3) = dbg s { _index = newIndex, _memory = newMemory }
  where newIndex = (_index s) + 4
        newMemory = insert resultIndex result (_memory s)
        resultIndex = getValueSafe s p3
        value1 = getValue s p1
        value2 = getValue s p2
        result = value1 + value2
        dbg x = trace ("ADD\t\t"++ show p1 ++ " [" ++ show value1 ++ "] + "++ show p2 ++" [" ++ show value2 ++ "] =\t[" ++ show result ++ "]\t-> @ [" ++ show resultIndex ++ "] " ++ show p3) x
-- OPCODE 2 - MULTIPLICATION
compute s (Instr 2 p1 p2 p3) = dbg s { _index = newIndex, _memory = newMemory }
  where newIndex = (_index s) + 4
        newMemory = insert resultIndex result (_memory s)
        resultIndex = getValueSafe s p3
        value1 = getValue s p1
        value2 = getValue s p2
        result = value1 * value2
        dbg x = trace ("MULT\t\t" ++ show p1 ++ " [" ++ show value1 ++ "] * "++ show p2++" [" ++ show value2 ++ "] =\t[" ++ show result ++ "]\t-> @ [" ++ show resultIndex ++ "] "++ show p3) x
-- OPCODE 3 - READ
compute s (Instr 3 p1 _ _) = dbg s { _index = newIndex, _memory = newMemory, _input = newInput }
  where newIndex = (_index s) + 2
        newMemory = insert resultIndex value (_memory s)
        resultIndex = getValueSafe s p1
        value = head $ _input s
        newInput = tail $ _input s
        dbg x = trace ("READ\t\t[" ++ show value ++ "]\t-> @ [" ++ show resultIndex ++ "] " ++ show p1) x
-- OPCODE 4 - OUTPUT
compute s (Instr 4 p1 _ _) = dbg s { _index = newIndex, _output = newOutput }
  where newIndex = (_index s) + 2
        newOutput = value:(_output s)
        value = getValue s p1
        dbg x = trace ("OUTPUT\t\t" ++ show p1 ++ " [" ++ show value ++ "]") x
-- OPCODE 5 - JUMP-IF-TRUE
compute s (Instr 5 p1 p2 _) = dbg s { _index = newIndex }
  where value = getValue s p1
        jumpTo = getValue s p2
        newIndex = if value /= 0 then jumpTo else (_index s) + 3
        dbg x = trace ("JUMP-IF-TRUE\t" ++ show p1 ++ " [" ++ show value ++ "] JMP TO " ++ show p2 ++ " [" ++ show jumpTo ++ "]\t-> INDEX " ++ show newIndex) x
-- OPCODE 6 - JUMP-IF-FALSE
compute s (Instr 6 p1 p2 _) = dbg s { _index = newIndex }
  where value = getValue s p1
        jumpTo = getValue s p2
        newIndex = if value == 0 then jumpTo else (_index s) + 3
        dbg x = trace ("JUMP-IF-FALSE\t" ++ show p1 ++ " [" ++ show value ++ "] JMP TO " ++ show p2 ++ " [" ++ show jumpTo ++ "]\t-> INDEX " ++ show newIndex) x
-- OPCODE 7 - LESS-THAN
compute s (Instr 7 p1 p2 p3) = dbg s { _index = newIndex, _memory = newMemory }
  where newIndex = (_index s) + 4
        value1 = getValue s p1
        value2 = getValue s p2
        resultIndex = getValueSafe s p3
        result = if value1 < value2 then 1 else 0
        newMemory = insert resultIndex result (_memory s)
        dbg x = trace ("LESS-THAN\t" ++ show p1 ++ " [" ++ show value1 ++ "] vs " ++ show p2 ++ " [" ++ show value2 ++ "] =\t[" ++ show result ++ "]\t-> @ [" ++ show resultIndex ++ "] "++ show p3) x
-- OPCODE 8 - EQUALS
compute s (Instr 8 p1 p2 p3) = dbg s { _index = newIndex, _memory = newMemory }
  where newIndex = (_index s) + 4
        value1 = getValue s p1
        value2 = getValue s p2
        result = if value1 == value2 then 1 else 0
        resultIndex = getValueSafe s p3
        newMemory = insert resultIndex result (_memory s)
        dbg x = trace ("EQUALS\t\t" ++ show p1 ++ " [" ++ show value1 ++ "] vs " ++ show p2 ++ " [" ++ show value2 ++ "] =\t[" ++ show result ++ "]\t-> @ [" ++ show resultIndex ++ "] "++ show p3) x
-- OPCODE 9 - REL-OFFSET
compute s (Instr 9 p1 _ _) = dbg s { _index = newIndex, _relIndex = newRelIndex }
  where value = getValue s p1
        newIndex = (_index s) + 2
        relIndex = _relIndex s
        newRelIndex = value + relIndex
        dbg x = trace ("REL-OFFSET\t" ++ show p1 ++ " [" ++ show value ++ "] + CUR [" ++ show relIndex ++ "] =\t[" ++ show newRelIndex ++ "]") x
-- OPCODE 99 - EXIT
compute s (Instr 99 _ _ _) = dbg s { _running = False }
  where dbg x = trace ("EXIT") x

-- ELSE: NOT HANDLED
compute s (Instr n _ _ _) = trace ("Unhandled opcode " ++ show n) s { _running = False }

runProgram :: State -> State
runProgram s
  | _running(s) == True = let instr = getInstruction s in runProgram (compute s instr)
  | otherwise = s

parseProgram :: String -> [Int]
parseProgram input = Prelude.map read (splitOn "," input)

-- Memory -> updated memory
runProgramV1 :: [Int] -> [Int]
runProgramV1 mem = Prelude.map snd . toList $ _memory endState
  where initialState = State 0 0 [] [] memory True
        endState = runProgram initialState
        memory = fromList $ zip (iterate (+1) 0) mem

-- Input -> memory -> output
runProgramV2 :: [Int] -> [Int] -> [Int]
runProgramV2 input mem = _output endState
  where initialState = State 0 0 input [] memory True
        endState = runProgram initialState
        memory = fromList $ zip (iterate (+1) 0) mem
