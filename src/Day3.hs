module Day3 where

import GHC.Exts

type Point = (Integer, Integer)

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

candidatesForValue :: (Floating a, RealFrac a) => a -> [Point]
candidatesForValue n =
  [(level, makeExpansion x level) | x <- [2, 4, 6, 8]]
  where
    level = levelForValue n

-- find the nearest candidate axis value, and return a distance to the center
-- equal to the level for that axis value plus the difference between
-- the axis value and the target value
distanceFromCandidates :: Integer
  -> [Point]
  -> Integer
distanceFromCandidates n candidates = abs (n - snd axisValue) + fst axisValue
  where
    axisValue = head $ sortWith (\x -> abs (n - snd x)) candidates

-- Part 2 -- first value bigger than my target value, 277678
-- in a more complicated puzzle

neighbor :: Point -> Point -> Bool
neighbor p1 p2 = abs (fst p1 - fst p2) <= 1 && abs (snd p1 - snd p2) <= 1

hasWestNeighbor :: Point -> [Point] -> Bool
hasWestNeighbor p points = elem (fst p - 1, snd p) points

hasEastNeighbor :: Point -> [Point] -> Bool
hasEastNeighbor p points = elem (fst p + 1, snd p) points

hasNorthNeighbor :: Point -> [Point] -> Bool
hasNorthNeighbor p points = elem (fst p, snd p + 1) points

hasSouthNeighbor :: Point -> [Point] -> Bool
hasSouthNeighbor p points = elem (fst p, snd p - 1) points

goEast :: [(Point, Integer)] -> [(Point, Integer)]
goEast locations =
  locations ++
  [(newpoint, sum $ fmap snd $ filter (\x -> neighbor newpoint (fst x)) locations)]
  where
    newpoint = (fst lastpoint + 1, snd lastpoint)
    lastpoint = fst $ last locations

goWest :: [(Point, Integer)] -> [(Point, Integer)]
goWest locations =
  locations ++
  [(newpoint, sum $ fmap snd $ filter (\x -> neighbor newpoint (fst x)) locations)]
  where
    newpoint = (fst lastpoint - 1, snd lastpoint)
    lastpoint = fst $ last locations

goNorth :: [(Point, Integer)] -> [(Point, Integer)]
goNorth locations =
  locations ++
  [(newpoint, sum $ fmap snd $ filter (\x -> neighbor newpoint (fst x)) locations)]
  where
    newpoint = (fst lastpoint, snd lastpoint + 1)
    lastpoint = fst $ last locations

goSouth :: [(Point, Integer)] -> [(Point, Integer)]
goSouth locations =
  locations ++
  [(newpoint, sum $ fmap snd $ filter (\x -> neighbor newpoint (fst x)) locations)]
  where
    newpoint = (fst lastpoint, snd lastpoint - 1)
    lastpoint = fst $ last locations

-- Probably there's a way to truth-table my way out of this mess MAYBE SOMEDAY
addLocations :: [(Point, Integer)] -> [(Point, Integer)]
addLocations [] = [((0, 0), 1)]
addLocations locations
  | mostRecent == ((0, 0), 1) = goEast locations -- when just starting
  | (not $ hasEastNeighbor point points)
    && (not $ hasSouthNeighbor point points)
    && (not $ hasWestNeighbor point points) = goEast locations -- when on the bottom left corner
  | (not $ hasEastNeighbor point points)
    && (not $ hasSouthNeighbor point points)
    && hasNorthNeighbor point points
    && hasWestNeighbor point points = goEast locations -- when in the bottom row
  | (not $ hasEastNeighbor point points)
    && (not $ hasSouthNeighbor point points)
    && (not $ hasNorthNeighbor point points) = goNorth locations -- when on the bottom right corner
  | (not $ hasEastNeighbor point points)
    && (not $ hasNorthNeighbor point points)
    && hasSouthNeighbor point points
    && hasWestNeighbor point points = goNorth locations -- when on the right column
  | (not $ hasEastNeighbor point points)
    && (not $ hasNorthNeighbor point points)
    && (not $ hasWestNeighbor point points) = goWest locations -- when on the top right corner
  | (not $ hasNorthNeighbor point points)
    && (not $ hasWestNeighbor point points)
    && hasEastNeighbor point points
    && hasSouthNeighbor point points = goWest locations -- when in the top row
  | (not $ hasNorthNeighbor point points)
    && (not $ hasWestNeighbor point points)
    && (not $ hasSouthNeighbor point points) = goSouth locations -- when in the top left corner
  | (not $ hasWestNeighbor point points)
    && (not $ hasSouthNeighbor point points)
    && hasEastNeighbor point points
    && hasNorthNeighbor point points = goSouth locations -- when in the left column
  | otherwise = locations
  where
    mostRecent = last locations
    points = fst <$> locations
    point = fst mostRecent

goUntil :: Integer -> Integer
goUntil n = go addLocations []
  where
    go _ [] = go addLocations (addLocations [])
    go f accum
      | lastSum > n = snd $ last accum
      | otherwise = go addLocations (addLocations accum)
      where lastSum = snd $ last accum
