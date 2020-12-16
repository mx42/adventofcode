module Y2015.Day13 (y15day13) where

import qualified Data.HashMap.Strict as HMap

import Data.List

type PeopleHappiness = HMap.HashMap (String, String) Int

-- Input line -> ((person 1 name, person 2 name), happiness)
parseLine :: String -> ((String, String), Int)
parseLine input  = ((p1, p2), signedNb)
  where input'   = words input
        p1       = input' !! 0
        p2       = init $ input' !! 10
        nb       = read $ input' !! 3 :: Int
        signedNb = case (input' !! 2) of
                     "gain"     -> nb
                     "lose"     -> 0 - nb
                     w          -> error ("invalid word at offset 2: " ++ w)

uniquePeople :: [((String, String), Int)] -> [String]
uniquePeople ps = nub $ concatMap (\((p1, p2), _) -> [p1, p2]) ps

-- Sum each couples of people to get an overall happiness score for any query order
combinePeople :: PeopleHappiness -> [((String, String), Int)] -> PeopleHappiness
combinePeople ph [] = ph
combinePeople ph (((p1, p2), h):t) = combinePeople newPH t
  where newPH = HMap.insert (p1, p2) newH $ HMap.insert (p2, p1) newH $ ph
        newH  = case (p1, p2) `HMap.lookup` ph of
                  Just h' -> h + h'
                  Nothing -> h

-- Get the total score of happiness for a given permutation of people
computeDispositionScore :: PeopleHappiness -> [String] -> Int
computeDispositionScore ph ps = endsScore + (fn ps)
  where endsScore     = ph HMap.! (head ps, last ps)
        fn (p1:p2:t)  = ph HMap.! (p1, p2) + (fn (p2:t))
        fn _          = 0

-- Add happiness mappings for me + each people
happinessWithMe :: [String] -> PeopleHappiness
happinessWithMe [] = HMap.empty
happinessWithMe (p:ps) = HMap.insert (p, "ME") 0 $ HMap.insert ("ME", p) 0 $ happinessWithMe ps

y15day13 :: [String] -> (String, String)
y15day13 input = (part1, part2)
  where part1  = show $ bestCombination
        part2  = show $ bestCombinationP2

        input' = map parseLine input

        -- Naive, "bruteforcing" implementation: we could at the very least avoid permutations that are simply rotations
        -- but given the low volume, it works... ¯\_(ツ)_/¯
        people = uniquePeople input'
        peopleHappiness = combinePeople HMap.empty input'
        bestCombination = maximum $ map (computeDispositionScore peopleHappiness) $ permutations people

        peopleP2 = "ME":people
        peopleHappinessP2 = HMap.union peopleHappiness $ happinessWithMe people
        bestCombinationP2 = maximum $ map (computeDispositionScore peopleHappinessP2) $ permutations peopleP2
