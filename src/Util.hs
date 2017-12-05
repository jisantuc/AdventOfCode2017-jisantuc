module Util where

import Text.Read

readInteger :: String -> Maybe Integer
readInteger x = readMaybe x

readInt :: String -> Maybe Int
readInt x = readMaybe x
