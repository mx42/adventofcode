module Y2020.Day21 (y20day21) where

import           Data.Function (on)
import qualified Data.Map as M
import           Data.List
import           Data.List.Split
import           Data.Maybe

parseEntry :: String -> ([String], [String])
parseEntry input    = (ingredients, allergens)
  where ingredients = words $ input' !! 0
        input'      = splitOn "(contains " input
        allergens   = splitOn ", " $ init $ input' !! 1

assoc  :: ([String], [String]) -> [(String, [[String]])]
assoc (ys, xs)  = map (\x -> (x, [ys])) xs

mostProbableIngredient :: [[String]] -> Maybe String
mostProbableIngredient [] = Nothing
mostProbableIngredient (is:[])
  | length is == 1 = Just $ head $ is
  | otherwise      = Nothing
mostProbableIngredient ilists
  | length mostIngr > 1 = Nothing
  | otherwise           = Just $ snd $ head $ mostIngr
  where uniqueIngr  = nub $ [i | is <- ilists, i <- is]
        ingrCount i = length $ filter (== True) $ [i `elem` is | is <- ilists]
        ingrCounts  = reverse $ sortBy (compare `on` fst) $ map (\i -> (ingrCount i, i)) $ uniqueIngr
        mostIngr    = filter (((==) $ fst $ head ingrCounts) . fst) $ ingrCounts

findIngrAllerg :: M.Map String [[String]] -> M.Map String String
findIngrAllerg allergMap
  | null allergMap   = M.empty
  | null foundValues = M.empty
  | otherwise        = M.union foundValues (findIngrAllerg remaining)
  where searchIngr   = M.map mostProbableIngredient allergMap
        foundValues  = M.fromList $ mapMaybe fn $ M.toList searchIngr
          where fn (_, Nothing) = Nothing
                fn (k, Just v)  = Just (k, v)
        remaining    = M.mapMaybeWithKey fn allergMap
          where fn :: String -> [[String]] -> Maybe [[String]]
                fn k a
                  | k `M.member` foundValues = Nothing
                  | otherwise                = Just $ map (filter (not . (`elem` foundValues))) $ a

y20day21 :: [String] -> (String, String)
y20day21 input = (part1, part2)
  where part1             = show $ length $ filter (not . (`elem` ingrWithAllergens)) $ concatMap fst $ products
        part2             = show $ sortedIngr
        products          = map parseEntry input
        allergMap         = findIngrAllerg $ M.fromListWith (++) $ concatMap assoc products
        ingrWithAllergens = M.elems allergMap
        sortedIngr        = intercalate "," $ map snd $ sortBy (compare `on` fst) $ M.toList $ allergMap
