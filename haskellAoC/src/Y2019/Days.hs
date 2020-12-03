module Y2019.Days (year2019) where

import Y2019.Day01
import Y2019.Day02
import Y2019.Day03
import Y2019.Day04
import Y2019.Day05
import Y2019.Day06
import Y2019.Day07
import Y2019.Day08
import Y2019.Day09
import Y2019.Day10
import Y2019.Day11
import Y2019.Day12
import Y2019.Day13

year2019 :: String -> [String] -> (String, String)
year2019 "01" = day1
year2019 "02" = day2
year2019 "03" = day3
year2019 "04" = day4
year2019 "05" = day5
year2019 "06" = day6
year2019 "07" = day7
year2019 "08" = day8
year2019 "09" = day9
year2019 "10" = day10
year2019 "11" = day11
year2019 "12" = day12
year2019 "13" = day13
