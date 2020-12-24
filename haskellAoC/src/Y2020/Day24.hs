module Y2020.Day24 (y20day24) where

import           Data.List
import qualified Data.Set as S

type Coords = (Int, Int, Int)

-- https://www.redblobgames.com/grids/hexagons/
--        0 -1 +1   +1 -1 0
--    -1  0 +1         +1 0 -1
--        -1 +1 0   0 +1 -1

neighbors :: Coords -> [Coords]
neighbors (x, y, z) = [ (x, y - 1, z + 1) -- NW
                      , (x + 1, y - 1, z) -- NE
                      , (x - 1, y, z + 1) -- W
                      , (x + 1, y, z - 1) -- E
                      , (x - 1, y + 1, z) -- SW
                      , (x, y + 1, z - 1) ] -- SE

parseLine :: String -> [Coords]
parseLine "" = []
parseLine ('n':'w':t) = (0, -1, 1) : parseLine t
parseLine ('n':'e':t) = (1, -1, 0) : parseLine t
parseLine ('s':'w':t) = (-1, 1, 0) : parseLine t
parseLine ('s':'e':t) = (0, 1, -1) : parseLine t
parseLine ('w':t)  = (-1, 0, 1) : parseLine t
parseLine ('e':t)  = (1, 0, -1) : parseLine t
parseLine str = error ("invalid prefix of string: " ++ str)

walkPath :: Coords -> [Coords] -> Coords
walkPath (x, y, z) ((x', y', z'):t) = walkPath (x + x', y + y', z + z') t
walkPath i [] = i

-- A bit slow solution (~1min) but acceptable for now...

nextDay :: S.Set Coords -> S.Set Coords
nextDay cs                    = S.filter shouldBeBlack allTiles
  where allTiles              = S.fromList $ concatMap neighbors cs
        countBlackNeighbors c = length $ filter (`elem` cs) $ neighbors c
        isBlack c             = c `elem` cs
        shouldBeBlack c       = case (isBlack c, countBlackNeighbors c) of
                                  (True,  1) -> True
                                  (True,  2) -> True
                                  (False, 2) -> True
                                  _          -> False

y20day24 :: [String] -> (String, String)
y20day24 input   = (part1, part2)
  where part1    = show $ length $ blackTiles
        part2    = show $ length $ head $ drop 100 $ iterate nextDay $ S.fromList blackTiles
        blackTiles = map head $ filter (odd . length) $ group $ sort $ map (walkPath (0,0,0) . parseLine) input
