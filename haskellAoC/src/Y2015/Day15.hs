module Y2015.Day15 (y15day15) where

import Data.List

data Ingredient = Ingredient
  { name       :: String
  , properties :: [Int]
  , calories   :: Int
  } deriving (Show)

parseLine :: String -> Ingredient
parseLine input = Ingredient n props cal
  where input'  = words $ filter (/= ',') input
        n       = init $ input' !! 0
        props   = map (read . (input' !!)) [2, 4, 6, 8]
        cal     = read $ input' !! 10

negIsZero :: Int -> Int
negIsZero n
  | n < 0 = 0
  | otherwise = n

computeScore :: [(Int, Ingredient)] -> Int
computeScore qtyIngr = product $ map (negIsZero) $ map (sum) $ transpose $ map ingredientProps qtyIngr
  where ingredientProps (qty, ingr) = map (* qty) $ properties ingr

computeScore2 :: [(Int, Ingredient)] -> Int
computeScore2 qtyIngr = product $ propertiesSums'
  where ingredientProps (qty, ingr) = map (* qty) $ (properties ingr ++ [calories ingr])
        has500cal = (last propertiesSums) == 500
        propertiesSums = map (negIsZero) $ map (sum) $ transpose $ map ingredientProps qtyIngr
        propertiesSums' = case has500cal of
          True -> init propertiesSums
          False -> [0]

getCombQty :: Int -> [Ingredient] -> [[(Int, Ingredient)]]
getCombQty 0 _ = []
getCombQty _ [] = []
getCombQty nb (i:[]) = [[(nb, i)]]
getCombQty nb (i:is) = concatMap (\qty -> map (\is' -> ((qty,i):is')) $ getCombQty (nb - qty) is) [1..nb]

y15day15 :: [String] -> (String, String)
y15day15 input = (part1, part2)
  where part1 = show $ maximum $ map computeScore $ combQties
        part2 = show $ maximum $ map computeScore2 $ combQties

        ingredients = map parseLine input
        combQties = getCombQty 100 ingredients
