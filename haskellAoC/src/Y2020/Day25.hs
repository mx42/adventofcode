module Y2020.Day25 (y20day25) where

doTransform :: Int -> Int -> Int
doTransform subjNumber n = rem (n * subjNumber) 20201227

getLoopSize :: Int -> Int
getLoopSize targetPK = length $ takeWhile (/= targetPK) $ iterate (doTransform 7) 1

transformNtimes :: Int -> Int -> Int
transformNtimes subjNumber loopSize = head $ drop loopSize $ iterate (doTransform subjNumber) 1

y20day25 :: [String] -> (String, String)
y20day25 input = (part1, part2)
  where part1  = show $ transformNtimes doorpk cardls
        part2  = show $ "WIP"
        input' = map read input :: [Int]
        doorpk = input' !! 0
        cardpk = input' !! 1
        -- doorls = getLoopSize doorpk
        cardls = getLoopSize cardpk
