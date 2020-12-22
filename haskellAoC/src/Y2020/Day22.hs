module Y2020.Day22 (y20day22) where

import qualified Data.Set as S
import Data.List.Split

type Setup = ([Int], [Int])

playRound :: Setup -> Setup
playRound ([], p2) = ([], p2)
playRound (p1, []) = (p1, [])
playRound ((p1:h1), (p2:h2))
  | p1 > p2   = (h1 ++ [p1, p2], h2)
  | p1 < p2   = (h1, h2 ++ [p2, p1])
  | otherwise = error ("Equality in a round (?!)")

computeScore :: [Int] -> Int
computeScore cards = sum $
                     map (uncurry (*)) $
                     zip (iterate (+1) 1) $
                     reverse $ cards

playCombat :: Setup -> Either Int Int
playCombat ([], p2) = Right $ computeScore p2
playCombat (p1, []) = Left  $ computeScore p1
playCombat hands    = playCombat $ playRound hands

playRecursiveCombat :: S.Set Setup -> Setup -> Either Int Int
playRecursiveCombat _ ([], p2)           = Right $ computeScore p2
playRecursiveCombat _ (p1, [])           = Left  $ computeScore p1
playRecursiveCombat previous setup@((p1:h1), (p2:h2))
  | setup `S.member` previous            = Left $ computeScore (h1 ++ [p1, p2])
  | p1 <= length h1 && p2 <= length h2   = case (playRecursiveCombat S.empty (take p1 h1, take p2 h2)) of
                                             Right _ -> playNext (h1, h2 ++ [p2, p1])
                                             Left  _ -> playNext (h1 ++ [p1, p2], h2)
  | otherwise                            = playNext $ playRound setup
  where playNext s   = playRecursiveCombat storedSetups s
        storedSetups = S.insert setup previous

y20day22 :: [String] -> (String, String)
y20day22 input = (part1, part2)
  where part1  = show $ playCombat (p1h, p2h)
        part2  = show $ playRecursiveCombat S.empty (p1h, p2h)

        hands' = splitOn [""] input
        p1h    = map read $ tail $ hands' !! 0 :: [Int]
        p2h    = map read $ tail $ hands' !! 1 :: [Int]
