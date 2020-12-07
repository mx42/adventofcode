module Y2015.Day07 (y15day07) where

import Debug.Trace

import           Data.Bits
import           Data.Char
import           Data.Function.Memoize
import           Data.Int
import           Data.List.Split
import qualified Data.Map as M

type Input = String
type Output = String
type Offset = Int

type Instruction = (Output, Wire)

data Wire = OpAnd Input Input |
  OpOr Input Input |
  OpNot Input |
  OpRshift Input Offset |
  OpLshift Input Offset |
  OpDirect Input deriving (Show)

parseInstruction :: String -> Instruction
parseInstruction input
  | len == 3 = ((parts !! 2), OpDirect (parts !! 0))
  | len == 4 = ((parts !! 3), OpNot (parts !! 1))
  | parts !! 1 == "RSHIFT" = ((parts !! 4), OpRshift (parts !! 0) (read $ parts !! 2))
  | parts !! 1 == "LSHIFT" = ((parts !! 4), OpLshift (parts !! 0) (read $ parts !! 2))
  | parts !! 1 == "AND" = ((parts !! 4), OpAnd (parts !! 0) (parts !! 2))
  | parts !! 1 == "OR" = ((parts !! 4), OpOr (parts !! 0) (parts !! 2))
  where parts = splitOn " " input
        len = length parts

getWireSignal :: M.Map Output Wire -> String -> Int
getWireSignal circuit wire = memo wire
  where memo = memoize getKey
        getKey k = case circuit M.! k of
                OpDirect val -> getVal val
                OpNot other -> complement $ getVal other
                OpOr i1 i2 -> (.|.) (getVal i1) (getVal i2)
                OpAnd i1 i2 -> (.&.) (getVal i1) (getVal i2)
                OpLshift inp ofs -> shiftL (getVal inp) ofs
                OpRshift inp ofs -> shiftR (getVal inp) ofs
        getVal k
          | all (isDigit) k = read k
          | otherwise = memo k

y15day07 :: [String] -> (String, String)
y15day07 input = (part1, part2)
  where part1 = signalA
        part2 = show $ getWireSignal instrP2 "a"
        instr = M.fromList $ map parseInstruction input
        signalA = show $ getWireSignal instr "a"
        instrP2 = M.insert "b" (OpDirect signalA) instr
