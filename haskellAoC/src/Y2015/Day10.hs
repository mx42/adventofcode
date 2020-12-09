module Y2015.Day10 (y15day10) where

import Data.List

lookAndSay :: String -> String
lookAndSay input = concat $ map describe_group $ group input
  where describe_group g = (show $ length g) ++ (head g):""


y15day10 :: [String] -> (String, String)
y15day10 (input:_) = (part1, part2)
  where part1 = show $ length $ part1resp
        part2 = show $ length $ head $ drop 10 $ iterate lookAndSay part1resp
        part1resp = head $ drop 40 $ iterate lookAndSay input
