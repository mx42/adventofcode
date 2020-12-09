module Y2020.Days (year2020) where

import Y2020.Day01
import Y2020.Day02
import Y2020.Day03
import Y2020.Day04
import Y2020.Day05
import Y2020.Day06
import Y2020.Day07
import Y2020.Day08
import Y2020.Day09

year2020 :: String -> [String] -> (String, String)
year2020 "01" = y20day01
year2020 "02" = y20day02
year2020 "03" = y20day03
year2020 "04" = y20day04
year2020 "05" = y20day05
year2020 "06" = y20day06
year2020 "07" = y20day07
year2020 "08" = y20day08
year2020 "09" = y20day09
