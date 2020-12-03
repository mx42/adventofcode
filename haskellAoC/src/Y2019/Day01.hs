module Y2019.Day01 (day1) where

massFuelReq :: Integer -> Integer
massFuelReq mass = (floor (fromIntegral mass / 3)) - 2

moduleFuelReq :: Integer -> Integer
moduleFuelReq mass
  | req > 0 =  req + moduleFuelReq req
  | req <= 0 = 0
  where req = massFuelReq mass

day1 :: [String] -> (String, String)
day1 input = (part1, part2)
  where
    modules = (map read input)
    part1 = show $ sum $ map massFuelReq modules
    part2 = show $ sum $ map moduleFuelReq modules
