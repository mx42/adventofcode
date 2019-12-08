module Geo.Point
  ( Point(..),
    origin,
    manhattanDist,
    add,
    sub
  ) where

data Point = Point { x :: Float, y :: Float } deriving (Show)

instance Eq Point where
  (==) (Point x1 y1) (Point x2 y2) = x1 == x2 && y1 == y2

origin :: Point
origin = Point 0 0

manhattanDist :: Point -> Point -> Float
manhattanDist a b = abs(x(b) - x(a)) + abs(y(b) - y(a))

add :: Point -> Point -> Point
add (Point xa ya) (Point xb yb) = Point (xa + xb) (ya + yb)

sub :: Point -> Point -> Point
sub (Point xa ya) (Point xb yb) = Point (xa - xb) (ya - yb)
