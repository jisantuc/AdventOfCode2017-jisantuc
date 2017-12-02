module Day2 where

import Util
import Data.Maybe

-- Part 1 -- checksums

lineToInts :: String -> [Integer]
lineToInts s = foldl (++) [] $ someInts
  where someInts = fmap (maybeToList . readInteger) $ words s

-- yeah yeah I know min and max exist as functions but if I do it this way
-- I can traverse the list only once. Getting into the spirit of Christmas,
-- or at least the spirit of puzzles, y'all
intsToRange :: [Integer] -> Integer
intsToRange (x:xs) = go 0 x x xs
  where
    go range _ _ [] = range
    go range min max (x:xs)
      | x < min = go (range - (x - min)) x max xs
      | x > max = go (range + (x - max)) min x xs
      | otherwise = go range min max xs

-- a "quotient buddy" is some x from a list of numbers that either divides
-- y evenly or is divided by y evenly
findQuotientBuddy :: Integer -> [Integer] -> Maybe Integer
findQuotientBuddy _ [] = Nothing
findQuotientBuddy d xs = listToMaybe $
  [ fst x | x <- (fmap (`divMod` d) xs ++ fmap (divMod d) xs), snd x == 0 ]

findEvenQuotients :: [Integer] -> Maybe Integer
findEvenQuotients [] = Nothing
findEvenQuotients (x:xs)
  | isJust $ quotientBuddy = quotientBuddy
  | otherwise = findEvenQuotients xs
  where
    quotientBuddy = findQuotientBuddy x xs

main = do
  inf <- readFile "puzzles/puzzle02.txt"
  numLines <- return $ fmap lineToInts $ lines inf
  rowRanges <- return $ fmap intsToRange numLines
  evenQuotients <- return $ fmap findEvenQuotients numLines
  print $ "Checksum: " ++ (show $ sum rowRanges)
  print $ "Sum of even quotients: " ++ (show $ fmap sum $ sequence evenQuotients)
