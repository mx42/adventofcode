import Data.List.Split

computeSides :: [Int] -> [Int]
computeSides dims = [l * w, w * h, h * l]
  -- Not really safe...
  where l = dims !! 0
        w = dims !! 1
        h = dims !! 2

computeWrapping :: [Int] -> Int
computeWrapping dims = sum (map (* 2) sides)
  where sides = computeSides dims

computeSlack :: [Int] -> Int
computeSlack dims = minimum sides
  where sides = computeSides dims

computeRibbonWrap :: [Int] -> Int
computeRibbonWrap dims = (sum dims - maximum dims) * 2

computeRibbonBow :: [Int] -> Int
computeRibbonBow dims = product dims

parseDims :: String -> [Int]
parseDims input = map read (splitOn "x" input)

part1computation :: [Int] -> Int
part1computation dims = computeWrapping dims + computeSlack dims

part2computation :: [Int] -> Int
part2computation dims = computeRibbonWrap dims + computeRibbonBow dims

day2 :: ([Int] -> Int) -> String -> Int
day2 fn input = sum paper
  where entries = lines input
        dims = map parseDims entries
        paper = map fn dims

main :: IO ()
main = do
  input <- getContents
  putStr "Part1: "
  print (day2 part1computation input)
  putStrLn ""
  putStr "Part2: "
  print (day2 part2computation input)
  putStrLn ""
