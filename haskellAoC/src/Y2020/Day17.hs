module Y2020.Day17 (y20day17) where

import qualified Data.HashMap.Strict as M

type Bounds = (Int, Int)
type Coords = (Int, Int, Int, Int)
type Board = M.HashMap Coords Bool

data State = State {
  _cycle :: Int        -- cycle number, for reference
  , _map :: Board      -- map of coords -> activity
  , _xs  :: Bounds     -- min and max rows with active stuff
  , _ys  :: Bounds     -- min and max cols with active stuff
  , _zs  :: Bounds     -- min and max layers with active stuff
  , _ws  :: Bounds     -- min and max ??? with active stuff
  } deriving (Show)

parseInput :: (Int, String) -> [(Coords, Bool)]
parseInput (y, str)         = map (\(x, b) -> ((x, y, 0, 0), b)) $ map parseChars $ zip (iterate (+1) 0) str
  where parseChars (x, '#') = (x, True)
        parseChars (x, _)   = (x, False)

getBounds :: Board -> (Bounds, Bounds, Bounds, Bounds)
getBounds board = foldl accumFn ((0, 0), (0, 0), (0, 0), (0, 0)) (M.keys board)
  where accumFn ((minx, maxx), (miny, maxy), (minz, maxz), (minw, maxw)) (x, y, z, w) =
          (
            (min minx x, max maxx x),
            (min miny y, max maxy y),
            (min minz z, max maxz z),
            (min minw w, max maxw w)
          )


countActiveNeighbors :: Board -> Coords -> Int
countActiveNeighbors b c = length $
                           filter (== True) $
                           map (\k -> M.lookupDefault False k b) $
                           getNeighbors c
  where getNeighbors (x, y, z, w) = filter (\n -> n /= c) $
                                    [(x', y', z', w') |
                                     x' <- [x - 1, x, x + 1],
                                     y' <- [y - 1, y, y + 1],
                                     z' <- [z - 1, z, z + 1],
                                     w' <- [w - 1, w, w + 1]]

newCycle :: Bool -> State -> State
newCycle wActive (State c b xb yb zb wb) = State c' b' xb' yb' zb' wb'
  where c'                   = c + 1
        (xb', yb', zb', wb') = getBounds b'
        b'                   = M.fromList $
                               filter (\(_, s) -> s == True) $
                               map (\coords -> (coords, activeness coords)) $
                               getCoordsList wActive
        getCoordsList False = [(x, y, z, 0) |
                                x <- [(fst xb - 1)..(snd xb + 1)],
                                y <- [(fst yb - 1)..(snd yb + 1)],
                                z <- [(fst zb - 1)..(snd zb + 1)]]
        getCoordsList True = [(x, y, z, w) |
                                x <- [(fst xb - 1)..(snd xb + 1)],
                                y <- [(fst yb - 1)..(snd yb + 1)],
                                z <- [(fst zb - 1)..(snd zb + 1)],
                                w <- [(fst wb - 1)..(snd wb + 1)]]
        oldState coords   = M.lookupDefault False coords b
        neighbors coords  = countActiveNeighbors b coords
        activeness coords = case (oldState coords, neighbors coords) of
                              (True, 2)  -> True
                              (True, 3)  -> True
                              (False, 3) -> True
                              _ -> False

countActive :: State -> Int
countActive (State _ b _ _ _ _) = M.size b

runNcycles :: Bool -> Int -> State -> State
runNcycles _ 0 s = s
runNcycles p c s = runNcycles p (c - 1) s'
  where s' = newCycle p s

y20day17 :: [String] -> (String, String)
y20day17 input = (part1, part2)
  where part1  = show $ countActive $ runNcycles False 6 initialState
        part2  = show $ countActive $ runNcycles True 6 initialState

        initialState =  State 0 initialMap xbounds ybounds zbounds wbounds
          where (xbounds, ybounds, zbounds, wbounds) = getBounds initialMap
        initialMap = M.fromList $ concatMap parseInput $ zip (iterate (+1) 0) input
