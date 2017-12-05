module Day5 where

import Util

data Jumper = Jumper Int [Int] deriving (Eq, Show)

incrementJumper :: Jumper -> Jumper
incrementJumper (Jumper idx instrs) =
  Jumper increment (getNewInstrs increment instrs)
  where
    increment = idx + instrs !! idx
    getNewInstrs :: Int -> [Int] -> [Int]
    getNewInstrs inc instrs =
      take idx instrs ++
      [(instrs !! idx) + 1] ++
      drop (idx + 1) instrs

-- keep jumping until out of the list
jump :: Jumper -> Maybe Jumper
jump j@(Jumper index instrs)
  | index < length instrs = Just $ incrementJumper j
  | otherwise = Nothing

countJumps :: Jumper -> Int
countJumps j = go 0 (jump j)
  where
    go :: Int -> Maybe Jumper -> Int
    go acc (Just jmpr) = go (acc + 1) (jump jmpr)
    go acc Nothing = acc

main = do
  inf <- readFile "puzzles/puzzle05.txt"
  steps <- return $ sequence $ fmap readInt $ lines inf
  jumper <- return $ fmap (Jumper 0) $ steps
  print $ "Number of steps to exit: " ++ (show $ fmap countJumps jumper)
