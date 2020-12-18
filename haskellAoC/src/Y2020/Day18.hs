module Y2020.Day18 (y20day18) where

import Data.Text (pack, unpack, replace)

-- Retrieve (if it exists) the first in-parenthesis expression it founds
getSubParen :: String -> String
getSubParen "" = ""
getSubParen ('(':s) = if '(' `notElem` s'
                      then s'
                      else getSubParen s'
  where s' = takeWhile (/= ')') s
getSubParen (_:s) = getSubParen s

-- Evaluate an expression (for part 1) left to right
-- Takes a wordified string to handle multi-character numbers...
evaluate :: [String] -> Int
evaluate input = fst $ foldl doOp (0, "") input
  where doOp (0, "")    c = (read c, "")
        doOp (res, "+") c = (res + (read c), "")
        doOp (res, "*") c = (res * (read c), "")
        doOp (res, "")  c = (res, c)
        doOp e c          = error ("Invalid fold on " ++ show (e,c) ++ " in " ++ input)

-- Evaluate an expression (for part 2) by doing +'s first then calling P1 evaluate function
evaluateP2 :: [String] -> Int
evaluateP2 input
  | "+" `elem` input = evaluateP2 $ removedPlus input
  | otherwise = evaluate input
  where removedPlus (a:"+":b:t) = (show ((read a :: Int) + (read b :: Int))):t
        removedPlus (x:t) = x:(removedPlus t)
        removedPlus [] = error ("Invalid formula, missing + operand in " ++ show input ++ "?")

-- Replace an expression with its result directly in string
replaceExpr :: Bool -> String -> String -> String
replaceExpr isP2 baseExp subExp = resultExp
  where toReplace          = pack $ "(" ++ subExp ++ ")"
        value False        = evaluate $ words subExp
        value True         = evaluateP2 $ words subExp
        replaceWith        = pack $ show $ value isP2
        resultExp          = unpack . replace toReplace replaceWith . pack $ baseExp

-- Main function: as long as there are parenthesis, evaluate those and replace them with their results
computeExpression :: Bool -> String -> Int
computeExpression isP2 input
  | '(' `elem` input   = computeExpression isP2 simplified
  | otherwise          = result isP2
    where simplified   = replaceExpr isP2 input $ getSubParen input
          result False = evaluate $ words input
          result True  = evaluateP2 $ words input

y20day18 :: [String] -> (String, String)
y20day18 input = (part1, part2)
  where part1  = show $ sum $ map (computeExpression False) input
        part2  = show $ sum $ map (computeExpression True)  input
