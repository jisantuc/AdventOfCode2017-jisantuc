module Day5 where

import Util

data Jumper = Jumper Int [Int] deriving (Eq, Show)

incrementJumper :: Int -> Jumper -> Jumper
incrementJumper inc (Jumper idx instrs) =
  Jumper increment (getNewInstrs increment instrs)
  where
    increment = idx + instrs !! idx
    getNewInstrs :: Int -> [Int] -> [Int]
    getNewInstrs incr instrs =
      take idx instrs ++
      [(instrs !! idx) + inc] ++
      drop (idx + 1) instrs

-- keep jumping until out of the list
jump :: Jumper -> Maybe Jumper
jump j@(Jumper index instrs)
  | index < length instrs = Just $ incrementJumper 1 j
  | otherwise = Nothing

-- do a normal jump if the increment is less than 3, otherwise,
-- decrease the value in that memory location by 1
jumpWeird :: Jumper -> Maybe Jumper
jumpWeird j@(Jumper index instrs)
  | present && positiveIdx && absInc < 3 = jump j
  | present && positiveIdx = Just $ incrementJumper (negate 1) j
  | otherwise = Nothing
  where
    present = index < length instrs
    positiveIdx = index >= 0
    absInc = (instrs !! index)

countJumps :: (Jumper -> Maybe Jumper) -> Jumper -> Int
countJumps f j = go 0 (f j)
  where
    go :: Int -> Maybe Jumper -> Int
    go acc (Just jmpr) = go (acc + 1) (f jmpr)
    go acc Nothing = acc

main = do
  inf <- readFile "puzzles/puzzle05.txt"
  steps <- return $ sequence $ readInt <$> lines inf
  jumper <- return $ (Jumper 0) <$> steps
  print $ "Number of normal steps to exit: " ++ (show $ (countJumps jump) <$> jumper)
  print $ "Number of weird steps to exit: " ++ (show $ (countJumps jumpWeird) <$> jumper)

