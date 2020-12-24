module Y2020.Day20 (y20day20) where

import           Data.Bits
import           Data.Function (on)
import           Data.List
import           Data.List.Split
import qualified Data.HashMap.Strict as M

type Coords = (Int, Int)

data Tile = Tile {
  tile_ids      :: [Int]
  , pixels      :: [[Bool]]
  , rows        :: Int
  , cols        :: Int
  , sides       :: [Int]
  } deriving (Show)


getIntValue :: [Bool] -> Int
getIntValue = fn 0
  where fn input [] = input
        fn input (b:bs) =
          case b of
            False -> fn shifted bs
            True  -> fn (shifted `setBit` 0) bs
          where shifted = shift input 1

getSides :: [[Bool]] -> [Int]
getSides input = sides' ++ reversed
  where input' = transpose input
        up     = head input
        down   = last input
        left   = head input'
        right  = last input'
        sides'   = map  getIntValue            $ [up, left, right, down]
        reversed = map (getIntValue . reverse) $ [up, left, right, down]

parseTile :: [String] -> Tile
parseTile []    = error ("Input error: no tile found")
parseTile (h:t) = makeTile [tid] pxs
  where tid       = read $ init $ (!! 1) $ words h
        pxs       = map (map (== '#')) t

makeTile :: [Int] -> [[Bool]] -> Tile
makeTile tids pxs = Tile tids pxs rs cs ss
  where rs        = length $ pxs
        cs        = length $ head $ pxs
        ss        = getSides pxs

getTilesWithSide :: [Tile] -> Int -> [Int]
getTilesWithSide tiles sideid = map (head . tile_ids) $ filter (elem sideid . sides) $ tiles

makeAdjacentPairs :: [Tile] -> [[Int]]
makeAdjacentPairs tiles = nub $                                         -- deduplicate
                          map (getTilesWithSide tiles) $                -- get associated tile_ids
                          map head $ filter ((== 2) . length) $         -- take only groups of 2
                          group $ sort $                                -- group them
                          concatMap sides tiles                         -- get all side "ids"

getTile :: [Tile] -> Int -> Tile
getTile tiles tile_id = head $ filter ((elem tile_id) . tile_ids) tiles

horizFlipPixels :: [[Bool]] -> [[Bool]]
horizFlipPixels px = map reverse px

vertFlipPixels :: [[Bool]] -> [[Bool]]
vertFlipPixels px = reverse px

rotateLeftPixels :: [[Bool]] -> [[Bool]]
rotateLeftPixels px = reverse $ transpose $ px

mergePixels :: Int -> [[Bool]] -> [[Bool]] -> [[Bool]]
-- could compare rows / cols lengths to avoid unnecessary rotations
mergePixels i a b
  | abot == btop = (init a) ++ (tail b)
  | bbot == atop = (init b) ++ (tail a)
  | otherwise  = mergePixels (i + 1) a' b'
    where atop = getIntValue $ head a
          abot = getIntValue $ last a
          btop = getIntValue $ head b
          bbot = getIntValue $ last b
          a'   = fn i
          fn 0 = a
          fn i'
            | i' `mod` 32 == 0 = vertFlipPixels a
            | i' `mod` 16 == 0 = horizFlipPixels a
            | i' `mod` 4  == 0 = rotateLeftPixels a
            | otherwise        = a
          b'   = rotateLeftPixels b

mergeTiles :: Tile -> Tile -> Tile
mergeTiles t1 t2 = makeTile tids pxs
  where tids     = concatMap tile_ids $ [t1, t2]
        pxs      = mergePixels 0 (pixels t1) (pixels t2)

buildTileLine :: [Tile] -> Tile
buildTileLine (h:[]) = h
buildTileLine (h:t)  = foldl mergeTiles h t
buildTileLine []     = error ("Can't build empty line!")

buildWholeMap :: [[Tile]] -> Tile
buildWholeMap [] = error ("Can't build empty tile map")
buildWholeMap m  = buildTileLine $ map buildTileLine m

--        adjacent pairs -> (corners, borders, innards)
classifyTiles :: [[Int]] -> ([Int], [Int], [Int])
classifyTiles adjPairs = foldl fn ([], [], []) $ group $ sort $ concat adjPairs
  where fn (cs, bs, ins) is@(i:_)
          | length is == 2 = (i:cs, bs, ins)
          | length is == 3 = (cs, i:bs, ins)
          | length is == 4 = (cs, bs, i:ins)
        fn acc _ = acc

common :: Ord a => Eq a => [a] -> [a] -> [a]
common xs ys = fn (sort xs) (sort ys)
  where fn [] _            = []
        fn _ []            = []
        fn (xh:xt) (yh:yt)
          | xh == yh       = xh:(common xt yt)
          | xh < yh        = common xt (yh:yt)
          | xh > yh        = common (xh:xt) yt

--      previous line -> adjacent -> start -> available -> finishing -> line
-- buildLineId :: [Int] -> [[Int]] -> Int -> [Int] -> [Int] -> [Int]
-- buildLineId _ _ _ _ [] = error ("No finish tile available?!")
-- buildLineId prevLine adj start available finishing = result
--     where adjTiles  = filter (/= start) $ concat $ filter (elem start) adj
--           adjFinish = common adjTiles finishing
--           adjAvail  = common adjTiles available
--           result    = case (adjFinish, adjAvail) of
--                         ([], (h:_)) -> h:(nextCall h)
--                         ((h:_), _)  -> h:[]
--                         _           -> error ("Error building line...")
--           nextCall tid = buildLineId adj tid (filter (/= tid) available) finishing

-- buildPicIds :: [[Int]] -> [[Int]]
-- buildPicIds adj      = adj
--   where (cs, bs, is) = classifyTiles adj
--         (c:cs')      = cs
--         firstLine    = buildLineId adj c bs cs'


y20day20 :: [String] -> (String, String)
y20day20 input = (part1, part2)
  where part1  = show $ product $ corners
        part2  = show $ "WIP"

        tiles    = map parseTile $ splitOn [""] input
        adjPairs = makeAdjacentPairs tiles
        (corners, _, _) = classifyTiles $ adjPairs

        -- tile n   = getTile tiles n
        -- wholePic = buildWholeMap $ map (map tile) $ buildPicIds adjPairs
