module Day1
  ( day1
  ) where

massFuelReq :: Integer -> Integer
massFuelReq mass = (floor (fromIntegral mass / 3)) - 2

moduleFuelReq :: Integer -> Integer
moduleFuelReq mass
  | req > 0 =  req + moduleFuelReq req
  | req <= 0 = 0
  where req = massFuelReq mass

day1 :: IO ()
day1 = do
  putStrLn "AoC 2019 day 1"
  putStr "Enter input >"
  input <- getContents
  putStrLn ""
  let modules = (map read (lines input))
  putStrLn "Part 1"
  print (sum (map massFuelReq modules))
  putStrLn "Part 2"
  print (sum (map moduleFuelReq modules))
