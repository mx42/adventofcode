module Y2020.Days (year2020) where

import Y2020.Day01
import Y2020.Day02
import Y2020.Day03
import Y2020.Day04


year2020 :: String -> [String] -> (String, String)
year2020 "01" = y20day01
year2020 "02" = y20day02
year2020 "03" = y20day03
year2020 "04" = y20day04
