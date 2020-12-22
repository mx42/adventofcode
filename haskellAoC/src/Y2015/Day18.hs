module Y2015.Day18 (y15day18) where

import Data.Maybe
import qualified Data.Set as S

type Coords = (Int, Int)
type Lights = S.Set Coords

rows :: Int
rows = 99

cols :: Int
cols = 99

corners :: [Coords]
corners = [(0, 0), (0, 99), (99, 0), (99, 99)]

parseLine :: (Int, String) -> [Coords]
parseLine (x, str) = concatMap (maybeToList . parseChar) $ zip (iterate (+1) 0) str
  where parseChar (y, '#') = Just (x, y)
        parseChar (_, '.') = Nothing
        parseChar entry    = error ("Error in input: " ++ show entry)

nbNeighbors :: Lights -> Coords -> Int
nbNeighbors ls (x, y)   = length $ filter (== True) $ map (`S.member` ls) $ neighborsCoords
  where neighborsCoords = [ (x - 1, y - 1), (x - 1, y), (x - 1, y + 1),
                            (    x, y - 1),             (    x, y + 1),
                            (x + 1, y - 1), (x + 1, y), (x + 1, y + 1) ]

nextStep :: Bool -> Lights -> Lights
nextStep isP2 lights = S.fromList $ concatMap (maybeToList . getNewState) $ allCoords
  where allCoords     = [(x, y) | x <- [0..cols], y <- [0..rows]]
        getNewState c
          | isP2 == True && c `elem` corners = Just c
          | otherwise = case (isLight, lightNeighbors) of
                          (True,  2) -> Just c
                          (_,  3)    -> Just c
                          _          -> Nothing
          where isLight        = c `S.member` lights
                lightNeighbors = nbNeighbors lights c

y15day18 :: [String] -> (String, String)
y15day18 input = (part1, part2)
  where part1  = show $ S.size $ head $ drop 100 $ iterate (nextStep False) state
        part2  = show $ S.size $ head $ drop 100 $ iterate (nextStep True) stateP2

        state   = S.fromList $ concatMap parseLine $ zip (iterate (+1) 0) input
        stateP2 = S.union state $ S.fromList corners
