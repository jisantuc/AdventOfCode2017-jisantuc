module Day1 where

import Data.Maybe
import Data.Semigroup

-- Part 1
-- sums of all numbers in a string of numbers that are the same as the next value
-- in the string

newtype CombinyInt = CombinyInt Integer deriving (Eq, Show)

instance Semigroup CombinyInt where
  (<>) x1 x2 = if x1 == x2 then x1 else CombinyInt 0

fromCombinyInt :: CombinyInt -> Integer
fromCombinyInt (CombinyInt x) = x

allNums :: String
allNums = "0123456789"

readInteger :: String -> Maybe Integer
readInteger "" = Nothing
readInteger x
  | elem (head x) allNums = Just (read x)
  | otherwise = Nothing

numsToNumList :: [CombinyInt] -> [Char] -> [CombinyInt]
numsToNumList accum [] = accum
numsToNumList accum (x : xs)
  | isJust (readInteger [x]) = numsToNumList (accum ++ [CombinyInt $ read [x]]) xs
  | otherwise = numsToNumList accum xs

sumRuns :: [CombinyInt] -> Integer
sumRuns nums = sum . map fromCombinyInt
  $ zipWith (<>) nums (tail nums ++ take 1 nums)

-- Part 2
-- the same, but for numbers halfway around the list
sumSymmetricalNums :: [CombinyInt] -> Integer
sumSymmetricalNums nums = sum . map fromCombinyInt
  $ zipWith (<>) nums (drop half nums ++ take half nums)
  where
    half = div (length nums) 2
