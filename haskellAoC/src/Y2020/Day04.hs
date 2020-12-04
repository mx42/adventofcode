module Y2020.Day04 (y20day04) where

import Data.Char
import Data.Maybe
import Data.List.Split
import qualified Data.Map as M

data RawPassport = RawPassport {
  birth_year :: String
  , issue_year :: String
  , expir_year :: String
  , height :: String
  , hair_color :: String
  , eye_color :: String
  , passport_id :: String
  } deriving (Show)

parsePassport :: String -> Maybe RawPassport
parsePassport input = do
  byr <- M.lookup "byr" mapped
  iyr <- M.lookup "iyr" mapped
  eyr <- M.lookup "eyr" mapped
  hgt <- M.lookup "hgt" mapped
  hcl <- M.lookup "hcl" mapped
  ecl <- M.lookup "ecl" mapped
  pid <- M.lookup "pid" mapped
  return (RawPassport byr iyr eyr hgt hcl ecl pid)
  where mapped = M.fromList $ map (\a -> (a !! 0, a !! 1)) $ map (splitOn ":") $ splitOn " " input

allTrue :: [Bool] -> Bool
allTrue = all (== True)

checkDigitInRange :: Int -> Int -> String -> Bool
checkDigitInRange mini maxi input = allTrue $ map ($ input) [(all (isDigit)), ((>= mini) . read), ((<= maxi) . read)]

validatePassport :: RawPassport -> Bool
validatePassport pass = allTrue [validatePassID, validateBirth, validateIssued, validateExpiration, validateHeight, validateHairColor, validateEyeColor]
  where validateBirth = checkDigitInRange 1920 2002 $ birth_year pass
        validateIssued = checkDigitInRange 2010 2020 $ issue_year pass
        validateExpiration = checkDigitInRange 2020 2030 $ expir_year pass
        validateHeight = not unitAvailable && (correctHeightCm || correctHeightIn)
          where correctHeightCm = checkDigitInRange 150 193 $ (splitOn "cm" (height pass)) !! 0
                correctHeightIn = checkDigitInRange 59 76 $ (splitOn "in" (height pass)) !! 0
                unitAvailable = all (isDigit) (height pass)
        validateHairColor = allTrue $ map ($ hair_color pass) [((== '#') . head), (all (`elem` "0123456789abcdef") . tail), ((== 7) . length)]
        validateEyeColor = (eye_color pass) `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        validatePassID = allTrue $ map ($ passport_id pass) [((== 9) . length), (all (isDigit))]

y20day04 :: [String] -> (String, String)
y20day04 input = (part1, part2)
  where part1 = show $ length $ passports
        part2 = show $ length $ validPassports
        passports = concatMap maybeToList $ map parsePassport $ map replaceNL $ splitOn "\n\n" $ unlines input
        replaceNL = map repl
        repl '\n' = ' '
        repl c = c
        validPassports = filter validatePassport passports
