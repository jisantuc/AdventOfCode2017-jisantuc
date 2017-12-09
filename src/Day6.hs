module Day6 where

import Data.List (sort, findIndex)

getIncrementingIndices :: Maybe Int -> Maybe Int -> Int -> [Int]
getIncrementingIndices Nothing _ _ = []
getIncrementingIndices _ Nothing _ = []
getIncrementingIndices (Just idx) (Just 0) _ = []
getIncrementingIndices (Just idx) (Just val) len = fmap (`mod` len) [idx+1..idx + val]

-- take a list length and a list of index locations to update and
-- return a list of that length with increments for those indices
buildIncrementList :: Int -> [Int] -> [Int]
buildIncrementList len [] = take len $ repeat 0
buildIncrementList len xs =
  [length $ filter (==i) xs | i <- [0..len-1]]

-- replace the value at an index in a list of integers with 0
zeroOut :: Maybe Int -> [Int] -> [Int]
zeroOut _ [] = []
zeroOut Nothing xs = xs
zeroOut (Just idx) xs
  | idx < length xs = take idx xs ++ [0] ++ drop (idx + 1) xs
  | otherwise = xs

valFromMaybeIdx :: Maybe Int -> [Int] -> Maybe Int
valFromMaybeIdx Nothing _ = Nothing
valFromMaybeIdx (Just x) [] = Nothing
valFromMaybeIdx (Just x) xs
  | x < length xs = Just $ xs !! x
  | otherwise = Nothing

cycleMem :: [Int] -> [Int]
cycleMem [] = []
cycleMem xs = zipWith (+) (zeroOut maxInd xs)
  $ buildIncrementList (length xs) indices
  where
    maxInd = findIndex (==(maximum xs)) xs
    indices =
      getIncrementingIndices maxInd (valFromMaybeIdx maxInd xs) (length xs)

detectRepeat :: Int -> [Int] -> [[Int]] -> Int
detectRepeat _ [] _ = 0
detectRepeat n xs cache =
  if (elem xs cache) then n
  else detectRepeat (n + 1) (cycleMem xs) (xs : cache)

detectRepeat2 :: Int -> [Int] -> [([Int], Int)] -> Int
detectRepeat2 _ [] _ = 0
detectRepeat2 n xs cache =
  if (elem xs $ fst <$> cache) then (n-) $ snd (head . filter (\x -> fst x == xs) $ cache)
  else detectRepeat2 (n + 1) (cycleMem xs) ((xs, n) : cache)

puzzle :: [Int]
puzzle = [4,10,4,1,8,4,9,14,5,1,14,15,0,15,3,5]

main = do
  "Size of loop: " ++ (show $ detectRepeat2 0 puzzle [])
