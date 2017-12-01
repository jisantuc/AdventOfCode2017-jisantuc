module Day1 where

import Data.Maybe

-- Part 1
-- sums of all numbers in a string of numbers that are the same as the next value
-- in the string

allNums :: String
allNums = "0123456789"

readInteger :: String -> Maybe Integer
readInteger "" = Nothing
readInteger x
  | elem (head x) allNums = Just (read x)
  | otherwise = Nothing

numsToNumList :: [Integer] -> [Char] -> [Integer]
numsToNumList accum [] = accum
numsToNumList accum (x : xs)
  | isJust (readInteger [x]) = numsToNumList (accum ++ [read [x]]) xs
  | otherwise = numsToNumList accum xs

wrappedValueFromNumString :: String -> Integer
wrappedValueFromNumString [] = 0
wrappedValueFromNumString (x : xs)
  | isJust (readInteger [x]) = read [x] :: Integer
  | otherwise = 0

sumRuns :: Integer -> Integer -> [Integer] -> Integer
sumRuns accum _ [] = accum
sumRuns accum initial (x: []) = if (x == initial) then accum + x else accum
sumRuns accum initial (x1 : x2 : xs) = if (x1 == x2)
  then (sumRuns (accum + x1) initial (x2:xs))
  else (sumRuns accum initial (x2:xs))
