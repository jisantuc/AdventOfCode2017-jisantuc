module Util where

import Text.Read

readInteger :: String -> Maybe Integer
readInteger x = readMaybe x
