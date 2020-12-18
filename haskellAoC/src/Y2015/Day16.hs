module Y2015.Day16 (y15day16) where

import qualified Data.HashMap.Strict as M

type Props = M.HashMap String Int

knownChars :: Props
knownChars = M.fromList [
  ("children", 3)
  , ("cats", 7)
  , ("samoyeds", 2)
  , ("pomeranians", 3)
  , ("akitas", 0)
  , ("vizslas", 0)
  , ("goldfish", 5)
  , ("trees", 3)
  , ("cars", 2)
  , ("perfumes", 1) ]

parseInput :: String -> (Int, Props)
parseInput input = (sueNb, M.fromList $ char rawChars)
  where input'   = words $ filter (/= ',') $ input
        sueNb    = read $ init $ input' !! 1
        rawChars = tail $ tail $ input'
        char (n:qty:t) = (init n, read qty):(char t)
        char _ = []

hasCorrespondingChars :: Props -> Bool
hasCorrespondingChars ps = M.foldlWithKey' checkKey True ps
  where checkKey False _ _ = False
        checkKey True  k v = v == (knownChars M.! k)

hasCorrespondingCharsP2 :: Props -> Bool
hasCorrespondingCharsP2 ps = M.foldlWithKey' checkKey True ps
  where checkKey False _ _ = False
        checkKey True  k v
          | k `elem` ["trees", "cats"] = v > (knownChars M.! k)
          | k `elem` ["pomeranians", "goldfish"] = v < (knownChars M.! k)
          | otherwise = v == (knownChars M.! k)

y15day16 :: [String] -> (String, String)
y15day16 input = (part1, part2)
  where part1  = show $ getTheReal hasCorrespondingChars
        part2  = show $ getTheReal hasCorrespondingCharsP2
        sues   = map parseInput input
        getTheReal fn = head $ map fst $ filter (\(_, p) -> fn p) $ sues
