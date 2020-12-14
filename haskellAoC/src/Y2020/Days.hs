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
import Y2020.Day10
import Y2020.Day11
import Y2020.Day12
import Y2020.Day13
import Y2020.Day14

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
year2020 "10" = y20day10
year2020 "11" = y20day11
year2020 "12" = y20day12
year2020 "13" = y20day13
year2020 "14" = y20day14
