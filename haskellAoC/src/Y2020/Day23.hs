module Y2020.Day23 (y20day23) where

import Data.Char

type Setup = [Int]

getDestination :: Int -> [Int] -> [Int] -> Int
getDestination dest pickup available
  | dest `elem` pickup    = getDestination (dest - 1) pickup available
  | dest == 0             = maximum available
  | dest `elem` available = dest
  -- | otherwise             = getDestination (dest - 1) pickup available

buildNewSetup :: Int -> [Int] -> Int -> [Int] -> Setup
buildNewSetup cur pickup dest (h:t)
  | h == dest  = (h:pickup) ++ t ++ [cur]
  | otherwise  = h:(buildNewSetup cur pickup dest t)
buildNewSetup _ _ _ [] = error ("invalid call (empty rest)")

playRound :: Setup -> Setup
playRound (current:t) = newSetup
  where pickup      = take 3 t
        rest        = drop 3 t
        destination = getDestination (current - 1) pickup rest
        newSetup    = buildNewSetup current pickup destination rest
playRound [] = error ("invalid setup (empty)")

getOrderAfter1 :: Setup -> String
getOrderAfter1 (1:r) = map intToDigit r
getOrderAfter1 (c:r) = getOrderAfter1 (r ++ [c])
getOrderAfter1 []    = error ("invalid setup (empty)")

get2after1 :: Setup -> [Int]
get2after1 (1:r) = take 2 r
get2after1 (_:r) = get2after1 r
get2after1 []    = error ("invalid setup (1 is missing ?!)")

y20day23 :: [String] -> (String, String)
y20day23 (input:_) = (part1, part2)
  where part1   = getOrderAfter1 $ head $ drop 100 $ iterate playRound p1setup
        part2   = "WIP"
        p1setup = map digitToInt input
