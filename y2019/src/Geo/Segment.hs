module Geo.Segment
  ( Segment(..),
    directionalVect,
    segmentIntersection,
    linkVectors,
    linkPoints,
    onSegment
  ) where

import Geo.Point
import Geo.Vector

data Segment = Segment { src :: Point, dst :: Point } deriving (Show)

directionalVect :: Segment -> Vector
directionalVect (Segment s d) = directionalVectorFromPoint (d `sub` s)

onSegment :: Point -> Segment -> Bool
onSegment p@(Point px py) (Segment s@(Point sx sy) d@(Point dx dy))
  | p == d = True
  | p == s = True
  | sx == dx && sx == px && (between py sy dy) = True
  | sy == dy && sy == py && (between px sx dx) = True
  | otherwise = False
  where between p' s' d' = if s' < d' then p' >= s' && p' <= d' else p' >= d' && p' <= s'

segmentIntersection :: Segment -> Segment -> Maybe Point
segmentIntersection s1 s2 =
  if t1 < 0 || t1 > 1 || t2 < 0 || t2 > 1
  then Nothing
  else Just (directionalVectorToPoint (Vector t1 t2) (src(s1)))
  where dirS1 = directionalVect s1
        dirS2 = directionalVect s2
        determinant = determinantVector dirS1 dirS2
        dirX = directionalVectorFromPoint (src(s2) `sub` src(s1))
        t1 = (determinantVector dirX dirS2) / determinant
        t2 = (determinantVector dirX dirS1) / determinant

linkPoints :: [Point] -> [Segment]
linkPoints pts = foldl addPointToSegments [] pts

linkVectors :: [Vector] -> [Segment]
linkVectors vectors = linkPoints (reverse points)
  where points = foldl linkVectors' [] vectors

linkVectors' :: [Point] -> Vector -> [Point]
linkVectors' [] v = [addV2P origin v]
linkVectors' xs@(p:_) v = (addV2P p v):xs

addPointToSegments :: [Segment] -> Point -> [Segment]
addPointToSegments [] pt = [Segment origin pt]
addPointToSegments xs@(h:_) pt = (Segment (dst(h)) pt):xs
