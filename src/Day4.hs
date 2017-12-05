module Day4 where

import Data.List

-- part 1
-- check that a string contains no duplicate words
isValid1 :: String -> Bool
isValid1 s = go True (words s)
  where
    go False _ = False
    go _ [] = True
    go True (w:ws) =
      go (not $ elem w ws) ws

-- part 2
-- check whether sorted string 1 is equal to sorted string 2
isAnagram :: String -> String -> Bool
isAnagram s1 s2 =
  sortBy compare s1 == sortBy compare s2

isValid2 :: String -> Bool
isValid2 s = go True (words s)
  where
    go False _ = False
    go _ [] = True
    go True (w:ws) =
      go (not $ any (isAnagram w) ws) ws

main = do
  inString <- readFile "puzzles/puzzle04.txt"
  validities1 <- return $ isValid1 <$> (lines inString)
  print $ "Number of valid lines, part 1: " ++ (show $ length $ filter id validities1)
  validities2 <- return $ isValid2 <$> (lines inString)
  print $ "Number of valid lines, part 2: " ++ (show $ length $ filter id validities2)

