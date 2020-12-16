module Y2015.Day12 (y15day12) where

import qualified Data.Aeson as JSON
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HMap
import qualified Data.Scientific as S
import Data.Char
import Data.Maybe
import Data.ByteString.Lazy.UTF8 (fromString)
import Debug.Trace

getStructureSum :: String -> Int
getStructureSum s = sum $ map read $ words $ map keepNum s
  where keepNum '-' = '-'
        keepNum c
          | isDigit c = c
          | otherwise = ' '

getJsonSum :: Int -> JSON.Value -> Int
getJsonSum n (JSON.Object o) =
  case isRed of
    True -> n
    False -> HMap.foldl' getJsonSum n o
  where isRed = any red values
        values = HMap.elems o
        red (JSON.String s) = (== "\"red\"") $ show s
        red _ = False
getJsonSum n (JSON.Array a) = Vector.foldl getJsonSum n a
getJsonSum n (JSON.Number nb) = n + intval
  where intval = case (S.toBoundedInteger nb :: Maybe Int) of
                   Just nb' -> nb'
                   Nothing -> 0
getJsonSum n _ = n

y15day12 :: [String] -> (String, String)
y15day12 (input:_) = (part1, part2)
  where part1 = show $ getStructureSum input
        part2 = show $ getJsonSum 0 $ fromJust jsonData
        jsonData = JSON.decode $ fromString input :: Maybe JSON.Value
