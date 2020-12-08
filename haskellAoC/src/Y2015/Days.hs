module Y2015.Days (year2015) where

import Y2015.Day01
import Y2015.Day02
import Y2015.Day03
import Y2015.Day04
import Y2015.Day05
import Y2015.Day06
import Y2015.Day07
import Y2015.Day08
import Y2015.Day09

year2015 :: String -> [String] -> (String, String)
year2015 "01" = y15day01
year2015 "02" = y15day02
year2015 "03" = y15day03
year2015 "04" = y15day04
year2015 "05" = y15day05
year2015 "06" = y15day06
year2015 "07" = y15day07
year2015 "08" = y15day08
year2015 "09" = y15day09
