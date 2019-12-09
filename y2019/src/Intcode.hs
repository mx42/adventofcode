module Intcode (computer, parseProgram, replaceNth) where

import Data.List.Split

-- import Debug.Trace
trace _ x = x

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs

-- computeOpCode: Take an INT and returns a tuple4:
-- - Int = opcode
-- - Subsequent bools = Mode of nth param (True = direct / False = pos mode)
computeOpCode :: Int -> (Int, Bool, Bool, Bool)
computeOpCode input = trace ("ComputeOpCode " ++ show input ++ " -> " ++ show result) result
  where opcode = input `mod` 100
        digits = show input
        len = length digits
        firstParamMode = if len > 2 then (digits !! (len - 3)) == '1' else False
        secondParamMode = if len > 3 then (digits !! (len - 4)) == '1' else False
        thirdParamMode = if len > 4 then (digits !! (len - 5)) == '1' else False
        result = (opcode, firstParamMode, secondParamMode, thirdParamMode)

getValue :: [Int] -> (Int, Bool) -> Int
getValue _ (n, True) = n
getValue r (n, False) = r !! n

-- opcode 1
opcodeAddition :: [Int] -> (Int, Int, Int) -> [Int]
opcodeAddition regs (operand1, operand2, resultIndex) = replaceNth resultIndex result regs
  where result = operand1 + operand2

-- opcode 2
opcodeMultiplication :: [Int] -> (Int, Int, Int) -> [Int]
opcodeMultiplication regs (operand1, operand2, resultIndex) = replaceNth resultIndex result regs
  where result = operand1 * operand2

-- opcode 7
opcodeLessThan :: [Int] -> (Int, Int, Int) -> [Int]
opcodeLessThan regs (operand1, operand2, resultIndex) = replaceNth resultIndex result regs
  where result = if operand1 < operand2 then 1 else 0

-- opcode 8
opcodeEqual :: [Int] -> (Int, Int, Int) -> [Int]
opcodeEqual regs (operand1, operand2, resultIndex) = replaceNth resultIndex result regs
  where result = if operand1 == operand2 then 1 else 0

computer :: [Int] -> [Int] -> [Int] -> Int -> ([Int], [Int])
computer readValues input output index
  | opcode == 1 = let newInput = opcodeAddition input (p1, p2, p3)
                      p1 = getValue' (val1, mode1)
                      p2 = getValue' (val2, mode2)
                      p3 = getValue' (val3, True)
                      newIndex = index + 4
                  in trace ("Opcode1 (add) (" ++ show p1 ++ ") + (" ++ show p2 ++ ") -> @" ++ show p3) computer readValues newInput output newIndex
  | opcode == 2 = let newInput = opcodeMultiplication input (p1, p2, p3)
                      p1 = getValue' (val1, mode1)
                      p2 = getValue' (val2, mode2)
                      p3 = getValue' (val3, True)
                      newIndex = index + 4
                  in trace ("Opcode2 (mult) (" ++ show p1 ++ ") * (" ++ show p2 ++ ") -> @" ++ show p3) (computer readValues newInput output newIndex)
  | opcode == 3 = let newInput = replaceNth p1 (head readValues) input
                      p1 = getValue' (val1, True)
                      newIndex = index + 2
                  in trace ("Opcode3 (read) (1) -> @" ++ show p1) computer (tail readValues) newInput output newIndex
  | opcode == 4 = let newOutput = p1:output
                      p1 = getValue' (val1, mode1)
                      newIndex = index + 2
                  in trace ("Opcode4 (output) " ++ show p1) computer readValues input newOutput newIndex
  | opcode == 5 = let p1 = getValue' (val1, mode1)
                      p2 = getValue' (val2, mode2)
                      newIndex = if p1 /= 0 then p2 else index + 3
                  in trace ("Opcode5 (jump-if-true) " ++ show p1 ++ " != 0 ? -> JMP @" ++ show p2) computer readValues input output newIndex
  | opcode == 6 = let p1 = getValue' (val1, mode1)
                      p2 = getValue' (val2, mode2)
                      newIndex = if p1 == 0 then p2 else index + 3
                  in trace ("Opcode6 (jump-if-false) " ++ show p1 ++ " == 0 ? -> JMP @" ++ show p2) computer readValues input output newIndex
  | opcode == 7 = let p1 = getValue' (val1, mode1)
                      p2 = getValue' (val2, mode2)
                      p3 = getValue' (val3, True)
                      newInput = opcodeLessThan input (p1, p2, p3)
                      newIndex = index + 4
                  in trace ("Opcode7 (less-than) " ++ show p1 ++ " < " ++ show p2 ++ " ? -> @" ++ show p3) computer readValues newInput output newIndex
  | opcode == 8 = let p1 = getValue' (val1, mode1)
                      p2 = getValue' (val2, mode2)
                      p3 = getValue' (val3, True)
                      newInput = opcodeEqual input (p1, p2, p3)
                      newIndex = index + 4
                  in trace ("Opcode7 (equal) " ++ show p1 ++ " == " ++ show p2 ++ " ? -> @" ++ show p3) computer readValues newInput output newIndex
  | otherwise = (input, output)
  where (opcode, mode1, mode2, mode3) = computeOpCode (input !! index)
        getValue' = getValue input
        val1 = input !! (index + 1)
        val2 = input !! (index + 2)
        val3 = input !! (index + 3)

parseProgram :: String -> [Int]
parseProgram input = map read (splitOn "," input)
