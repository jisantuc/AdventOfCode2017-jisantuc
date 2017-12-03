module Day3 where

import GHC.Exts

-- Get the value along any axis or diagonal for a given square size
makeExpansion :: Integer -> Integer -> Integer
makeExpansion n 1 = n
makeExpansion _ 0 = 1
makeExpansion n level = (n - 1) + 8 * (level - 1) + makeExpansion n (level - 1)

minSqGreaterThanN :: (Floating a, RealFrac a) => a -> Integer
minSqGreaterThanN n = ceiling $ sqrt n

squareLevelFromMinSq :: Integer -> Integer
squareLevelFromMinSq n
  | even n = div n 2
  | otherwise = div (n - 1) 2

levelForValue :: (Floating a, RealFrac a) => a -> Integer
levelForValue n = squareLevelFromMinSq $ minSqGreaterThanN n

candidatesForValue :: (Floating a, RealFrac a) => a -> [(Integer, Integer)]
candidatesForValue n =
  [(level, makeExpansion x level) | x <- [2, 4, 6, 8]]
  where
    level = levelForValue n

-- find the nearest candidate axis value, and return a distance to the center
-- equal to the level for that axis value plus the difference between
-- the axis value and the target value
distanceFromCandidates :: Integer
  -> [(Integer, Integer)]
  -> Integer
distanceFromCandidates n candidates = abs (n - snd axisValue) + fst axisValue
  where
    axisValue = head $ sortWith (\x -> abs (n - snd x)) candidates
