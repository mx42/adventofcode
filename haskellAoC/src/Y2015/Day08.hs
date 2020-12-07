module Y2015.Day08 (y15day08) where

import Data.List

decodeStr :: String -> String
decodeStr = dropFirstLast . removeStuff
  where dropFirstLast = tail . init
        removeStuff s@(h:t)
          | "\\x" `isPrefixOf` s  = removeStuff (drop 3 s)
          | "\\\\" `isPrefixOf` s = h:(removeStuff (drop 1 t))
          | "\\" `isPrefixOf` s   = removeStuff t
          | otherwise             = h:(removeStuff t)
        removeStuff ""            = ""

encodeStr :: String -> String
encodeStr input = "\"" ++ (encode' input) ++ "\""
  where encode' (h:t)
          | h == '\\' = "\\\\" ++ (encode' t)
          | h == '"' = "\\\"" ++ (encode' t)
          | otherwise = h:(encode' t)
        encode' "" = ""

y15day08 :: [String] -> (String, String)
y15day08 input = (part1, part2)
  where part1 = show $ (raw_length - decoded_length)
        part2 = show $ (encoded_length - raw_length)
        decoded_length = sum $ map (length . decodeStr) input
        encoded_length = sum $ map (length . encodeStr) input
        raw_length = sum $ map length input
