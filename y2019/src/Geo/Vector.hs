module Geo.Vector
  ( Vector(..),
    determinantVector,
    directionalVectorFromPoint,
    directionalVectorToPoint,
    nilVector,
    addV2P
  ) where

import Geo.Point

data Vector = Vector { x :: Float, y :: Float } deriving (Show)

addV2P :: Point -> Vector -> Point
addV2P (Point xp yp) (Vector xv yv) = Point (xp + xv) (yp + yv)

nilVector :: Vector
nilVector = Vector 0 0

directionalVectorFromPoint :: Point -> Vector
directionalVectorFromPoint (Point px py) = Vector (px / len) (py / len)
  where len = sqrt(px * px + py * py)

determinantVector :: Vector -> Vector -> Float
determinantVector (Vector x1 y1) (Vector x2 y2) = (x1 * y2) - (y1 * x2)

directionalVectorToPoint :: Vector -> Point -> Point
directionalVectorToPoint (Vector vx vy) (Point px py) = Point (vx * px) (vy * py)
