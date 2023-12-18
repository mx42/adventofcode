module Day07 (day07) where

import Data.List (sort, sortBy, group)
import Data.List.Split (splitOn)


data HandScore = HandScore { typeValue :: Int, highCard1 :: Int, highCard2 :: Int, highCard3 :: Int, highCard4 :: Int, highCard5 :: Int } deriving (Show, Eq, Ord)

type Hand = [Int]
type Bid = Int

-- HandScore      typeValue     highCard1       highCard2
-- 5 of a kind        10        card value      0
-- 4 of a kind         9        card value      remaining
-- full house          8        3 card value    2 card value
-- 3 of a kind         7        3 card value    remaining best
-- 2 pairs             6        2 card value1   2 card value2
-- 1 pair              5        2 card value    remaining best
-- high card           1        best value      2nd best
--getHandScore :: Hand -> HandScore
--getHandScore cards = score
--  where grouped = group cards

compareGroupOfCards :: [Int] -> [Int] -> Ordering
compareGroupOfCards x y
  | lengths == EQ = vals
  | otherwise = lengths
  where lengths = compare (length y) (length x)
        vals = compare (head y) (head x)


getHandScore :: Hand -> HandScore
getHandScore cards
  | length grouped == 1 = HandScore 10 (head cards) (cards !! 1) (cards !! 2) (cards !! 3) (cards !! 4)
  | length grouped == 2 && length (head grouped) == 4 = HandScore 9 (head cards) (cards !! 1) (cards !! 2) (cards !! 3) (cards !! 4)
  | length grouped == 2 && length (head grouped) == 3 = HandScore 8 (head cards) (cards !! 1) (cards !! 2) (cards !! 3) (cards !! 4)
  | length grouped == 3 && length (head grouped) == 3 = HandScore 7 (head cards) (cards !! 1) (cards !! 2) (cards !! 3) (cards !! 4)
  | length grouped == 3 && length (head grouped) == 2 = HandScore 6 (head cards) (cards !! 1) (cards !! 2) (cards !! 3) (cards !! 4)
  | length grouped == 4 = HandScore 5 (head cards) (cards !! 1) (cards !! 2) (cards !! 3) (cards !! 4)
  | otherwise = HandScore 1 (head cards) (cards !! 1) (cards !! 2) (cards !! 3) (cards !! 4)
  where grouped = sortBy compareGroupOfCards $ group $ sort cards

getHandScoreP2 :: Hand -> HandScore
getHandScoreP2 cards
  | length newGrouped == 1 = HandScore 10 (head cards) (cards !! 1) (cards !! 2) (cards !! 3) (cards !! 4)
  | length newGrouped == 2 && length (head newGrouped) == 4 = HandScore 9 (head cards) (cards !! 1) (cards !! 2) (cards !! 3) (cards !! 4)
  | length newGrouped == 2 && length (head newGrouped) == 3 = HandScore 8 (head cards) (cards !! 1) (cards !! 2) (cards !! 3) (cards !! 4)
  | length newGrouped == 3 && length (head newGrouped) == 3 = HandScore 7 (head cards) (cards !! 1) (cards !! 2) (cards !! 3) (cards !! 4)
  | length newGrouped == 3 && length (head newGrouped) == 2 = HandScore 6 (head cards) (cards !! 1) (cards !! 2) (cards !! 3) (cards !! 4)
  | length newGrouped == 4 = HandScore 5 (head cards) (cards !! 1) (cards !! 2) (cards !! 3) (cards !! 4)
  | otherwise = HandScore 1 (head cards) (cards !! 1) (cards !! 2) (cards !! 3) (cards !! 4)
  where grouped = sortBy compareGroupOfCards $ group $ sort cards
        groupOf1 = filter (\gc -> head gc == 1) grouped
        jokers = if (length groupOf1) > 0 then head groupOf1 else []
        groupDiff1 = filter (\gc -> head gc /= 1) grouped
        newGrouped = (head groupDiff1 ++ jokers) : (if length groupDiff1 > 0 then tail groupDiff1 else []) 

charToCard :: Char -> Int
charToCard '2' = 2
charToCard '3' = 3
charToCard '4' = 4
charToCard '5' = 5
charToCard '6' = 6
charToCard '7' = 7
charToCard '8' = 8
charToCard '9' = 9
charToCard 'T' = 10
charToCard 'J' = 11
charToCard 'Q' = 12
charToCard 'K' = 13
charToCard 'A' = 14
charToCard _   = 0

parseLine :: String -> (String, Hand, Bid, HandScore)
parseLine input = (head input', hand, bid, getHandScore hand)
  where input' = splitOn " " input          :: [String]
        hand = map charToCard (head input') :: [Int]
        bid = read $ last input'            :: Bid

mapForP2 :: (Int, String, Hand, Bid, HandScore) -> (String, Hand, Bid, HandScore)
mapForP2 (_, str, h1, bid, _) = (str, h2, bid, s2)
  where h2 = map (\c -> if c == 11 then 1 else c) h1
        s2 = getHandScoreP2 h2

addRank :: [(String, Hand, Bid, HandScore)] -> [(Int, String, Hand, Bid, HandScore)]
addRank entries = map (\(rank, (str, hand, bid, score)) -> (rank, str, hand, bid, score)) entriesWithRank
  where entriesWithRank = zip [1..] sortedEntries
        sortedEntries = sortBy (\(_, _, _, s1) (_, _, _, s2) -> compare s1 s2) entries 

day07 :: IO ()
day07 = do
  putStrLn "AOC 2023 day 07"
  input <- getContents
  let hands = addRank $ map parseLine $ lines input
  let resP1 = sum $ map (\(rank, _, _, bid, _) -> rank * bid) hands
  putStrLn "Part1"
  print resP1
  let handP2 = addRank $ map mapForP2 hands
  putStrLn "Part2"
  mapM_ print handP2
  let resP2 = sum $ map (\(rank, _, _, bid, _) -> rank * bid) handP2
  print resP2
