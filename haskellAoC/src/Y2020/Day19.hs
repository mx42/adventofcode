module Y2020.Day19 (y20day19) where

import qualified Data.IntMap as M
import           Data.List.Split

data Rule = AndRule Rule Rule |
            OrRule Rule Rule |
            RefRule Int |
            CharRule Char deriving (Show)

parseRule :: String -> (Int, Rule)
parseRule input = (ruleId, rule)
  where rule    = parseFn $ parts !! 1
        ruleId  = read $ parts !! 0
        parts   = splitOn ": " input
        parseFn ['"', c, '"'] = CharRule c
        parseFn str
          | '|' `elem` str = OrRule (parseFn $ pipeSplit !! 0) (parseFn $ pipeSplit !! 1)
          | ' ' `elem` str = AndRule (parseFn $ spaceSplit !! 0) (parseFn $ spaceSplit !! 1)
          | otherwise      = RefRule (read str)
          where spaceSplit = words str
                pipeSplit  = splitOn " | " str

matchRule :: M.IntMap Rule -> Int -> String -> Bool
matchRule ruleset ruleId input = "" `elem` (fn rule0 input)
  where rule0 = ruleset M.! ruleId
        fn :: Rule -> String -> [String]
        fn _ ""           = []
        fn rule str@(h:t) =
          case rule of
            CharRule c -> if h == c
                          then [t]
                          else []
            RefRule r     -> fn (ruleset M.! r) str
            OrRule  r1 r2 -> (fn r1 str) ++ (fn r2 str)
            AndRule r1 r2 -> [p2 | p1 <- fn r1 str, p2 <- fn r2 p1]


y20day19 :: [String] -> (String, String)
y20day19 input  = (part1, part2)
  where part1   = show $ length $ filter (matchRule rules 0) $ entries
        part2   = show $ length $ filter (matchRule rulesP2 0) $ entries
        input'  = splitOn [""] input
        rules   = M.fromList $ map parseRule $ input' !! 0
        entries = input' !! 1
        rulesP2 = M.insert 8 (OrRule (RefRule 42) (AndRule (RefRule 42) (RefRule 8))) $
                  M.insert 11 (OrRule (AndRule (RefRule 42) (RefRule 31))
                               (AndRule (RefRule 42) (AndRule (RefRule 11) (RefRule 31)))) $
                  rules
