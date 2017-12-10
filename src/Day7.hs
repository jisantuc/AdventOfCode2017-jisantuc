module Day7 where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.Combinator as Comb

import Data.Char
import Control.Applicative

data Node =
  Node { children :: [String]
       , name :: String
       , weight :: Int }
  | Empty deriving (Eq, Show)

hasChildren :: Node -> Bool
hasChildren n = (length . children) n > 0

parseNodeLine :: Parser Node
parseNodeLine = do
  name <- manyTill anyChar (string " (")
  weight <- decimal
  char ')'
  childNames <-
    (string " -> "
    *> Comb.sepBy (manyTill anyChar endOfLine) (string ", "))
    <|> endOfLine *> pure []
  return $ Node childNames name weight

parseNodeLineFile :: Parser [Node]
parseNodeLineFile =
  Comb.many1 $ parseNodeLine

getNodeList :: Either a [Node] -> [Node]
getNodeList (Left _) = []
getNodeList (Right ns) = ns

findRootNode :: [Node] -> Node
findRootNode [] = Empty
findRootNode (n:ns)
  | (elem (name n)) (concat $ children <$> ns) = findRootNode ns
  | otherwise = n

nodeNameToNode :: BS.ByteString -> [BS.ByteString] -> Parser Node
nodeNameToNode = undefined

treeFromNodeList :: Node -> [Node] -> Node
treeFromNodeList _ [] = Empty
treeFromNodeList root (ns) = undefined

main = do
  inf <- BS.readFile "puzzles/puzzle07.txt"
  nodes <- return $ getNodeList $ parseOnly parseNodeLineFile inf
  nodesWithChildren <- return $ filter hasChildren nodes
  rootNode <- return $ findRootNode nodesWithChildren
  print $ "Number of nodes: " ++ (show $ length nodes)
  print $ "Number of nodes with children: " ++ (show $ length nodesWithChildren)
  return $ name rootNode
