module Y2020.Day08 (y20day08) where

import Data.List
import Data.List.Split
import qualified Data.Map as M

data Instr = Acc Int | Jmp Int | Nop Int deriving (Show)

-- current offset, current acc, visited offsets
type State = (Int, Int, [Int])

runInstructions :: M.Map Int Instr -> State -> (Bool, Int)
runInstructions instr (cur, acc, visited)
  | cur `elem` visited           = (False, acc)   -- looping
  | cur `M.notMember` instr      = (True, acc) -- terminated
  | otherwise                    = runInstructions instr (newCur, newAcc, newVisited)
  where newVisited       = cur:visited
        (newCur, newAcc) = case (instr M.! cur) of
                             Nop _      -> (cur + 1, acc)
                             Acc qty    -> (cur + 1, acc + qty)
                             Jmp ofs    -> (cur + ofs, acc)

parseInput :: String -> Instr
parseInput input
  | "nop" `isPrefixOf` input = Nop nb
  | "acc" `isPrefixOf` input = Acc nb
  | "jmp" `isPrefixOf` input = Jmp nb
  where nb                    = (sign raw_sign) 0 number :: Int
        (raw_sign:raw_number) = (splitOn " " input) !! 1
        sign '+'              = (+)
        sign '-'              = (-)
        number                = read raw_number :: Int

testSwappedNopJmp :: M.Map Int Instr -> Int -> Int
testSwappedNopJmp original offset = case original M.! offset of
                                      Acc _     -> next
                                      Jmp qty   -> resWithSwap (Nop qty)
                                      Nop qty   -> resWithSwap (Jmp qty)
  where next                     = testSwappedNopJmp original (offset + 1)
        resWithSwap swapped      = case (runInstructions (swappedInput swapped) (0, 0, [])) of
                                     (True, acc) -> acc
                                     (False, _) -> next
        swappedInput new         = M.insert offset new original

y20day08 :: [String] -> (String, String)
y20day08 input = (part1, part2)
  where part1    = show $ snd $ runInstructions ops (0, 0, [])
        part2    = show $ testSwappedNopJmp ops 0
        ops      = M.fromList $ zip (iterate (+ 1) 0) $ map parseInput input
