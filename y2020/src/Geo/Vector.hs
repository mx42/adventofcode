module Geo.Vector
  ( Vector(..),
    determinantVector,
    directionalVectorFromPoint,
    directionalVectorToPoint,
    nilVector,
    addV2P,
    roundVecAtN,
    roundN,
    vec2angle
  ) where

import Geo.Point

data Vector = Vector { x :: Double, y :: Double } deriving (Show, Eq)

addV2P :: Point -> Vector -> Point
addV2P (Point xp yp) (Vector xv yv) = Point (xp + xv) (yp + yv)

nilVector :: Vector
nilVector = Vector 0 0

roundVecAtN :: Int -> Vector -> Vector
roundVecAtN prec (Vector vx vy) = Vector (roundN prec vx) (roundN prec vy)

-- https://stackoverflow.com/questions/12450501/round-number-to-specified-number-of-digits
roundN :: Int -> Double -> Double
roundN prec val = (fromInteger $ round $ val * (10 ^ prec)) / (10.0 ^^ prec)

directionalVectorFromPoint :: Point -> Vector
directionalVectorFromPoint (Point px py) = Vector (px / len) (py / len)
  where len = sqrt(px * px + py * py)

determinantVector :: Vector -> Vector -> Double
determinantVector (Vector x1 y1) (Vector x2 y2) = (x1 * y2) - (y1 * x2)

directionalVectorToPoint :: Vector -> Point -> Point
directionalVectorToPoint (Vector vx vy) (Point px py) = Point (vx * px) (vy * py)

vec2angle :: Vector -> Double
vec2angle (Vector vx vy) = roundN 5 angle'
 where angle = atan2 vx vy
       angle' = pi - angle
