module Y2020.Day16 (y20day16) where

import Data.List.Split
import Data.List

data Field = Field {label :: String, validateFn :: Int -> Bool}

parseField :: String -> Field
parseField input = Field lbl validateInput
  where input'   = splitOn ":" input

        lbl             = input' !! 0
        [p1, "or", p2]  = words $ input' !! 1

        validateInput i = (validateRange p1 i) || (validateRange p2 i)
        validateRange strRange i = i >= mini && i <= maxi
          where [mini, maxi] = map read $ splitOn "-" strRange

checkFields :: [Field] -> Int -> [String]
checkFields [] _ = []
checkFields ((Field lbl v):fs) tkt
  | (v tkt) == True = lbl:(checkFields fs tkt)
  | otherwise = checkFields fs tkt

parseTicket :: String -> [Int]
parseTicket s = map read $ splitOn "," s

ticketIsValid :: [Field] -> [Int] -> Bool
ticketIsValid valids fields = (== 0) . length $ filter ((== 0) . length) $ map (checkFields valids) fields

-- validation function
-- values (without indices)
-- valid indices
validIndices :: (Int -> Bool) -> [Int] -> [Int]
validIndices fn tkt = map fst $ filter (\(k, v) -> v == True) $ zip (iterate (+1) 0) $ map fn tkt

fieldIndices :: [[Int]] -> Field -> [Int]
fieldIndices tkts (Field _ fn) = common $ map (validIndices fn) tkts
  where common [] = []
        common (l1:ls) = foldl intersect l1 ls

getFieldsIndices :: [(String, [Int])] -> [(String, Int)]
getFieldsIndices [] = []
getFieldsIndices input = (lbl, index):remaining
  where getFirstOnlyIndex ((l, is):t)
          | length is == 1 = (l, head is)
          | otherwise = getFirstOnlyIndex t
        (lbl, index) = getFirstOnlyIndex input
        remaining = getFieldsIndices $ map filterOutIndex $ filter (\(l, _) -> l /= lbl) input
        filterOutIndex (l, is') = (l, filter (\i -> i /= index) is')

y20day16 :: [String] -> (String, String)
y20day16 input = (part1, part2)
  where part1 = show $ sum $ filter (\v -> (== 0) $ length $ checkFields fields v) $ concat nearbyTickets
        part2 = show $ product $ map ((!!) myTicket) $ map snd $ filter (\(l, _) -> "departure" `isPrefixOf` l) $ fieldsIndices

        [raw_fields, raw_myTicket, raw_nearbyTickets] = splitOn [""] input
        fields = map parseField raw_fields
        myTicket = parseTicket $ raw_myTicket !! 1
        nearbyTickets = map parseTicket $ tail $ raw_nearbyTickets

        validNearbyTickets = filter (ticketIsValid fields) nearbyTickets
        fieldsIndices = getFieldsIndices $ map (\f -> (label f, fieldIndices validNearbyTickets f)) $ fields
