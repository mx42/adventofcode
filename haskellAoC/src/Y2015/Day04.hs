module Y2015.Day04 (y15day04) where

import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.MD5

getLeadingZeroes :: String -> Int -> Int
getLeadingZeroes base nb = nbZeroes
  where pass = base ++ (show nb)
        md5hash = show $ md5 $ fromString pass
        nbZeroes = length $ takeWhile (== '0') md5hash

-- Crappy bruteforce solution...
y15day04 :: [String] -> (String, String)
y15day04 (input:_) = (part1, part2)
  where part1 = show $ firstWithNzeroes 5
        part2 = show $ firstWithNzeroes 6
        firstWithNzeroes n = head $ take 1 $ dropWhile (\h -> getLeadingZeroes input h /= n) $ iterate (+1) 0
