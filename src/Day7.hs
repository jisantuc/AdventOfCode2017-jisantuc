module Day7 where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.Combinator as Comb

import Data.Char
import Control.Applicative

data Node =
  Node { children :: [BS.ByteString]
       , name :: BS.ByteString
       , weight :: Int }
  | Empty deriving (Eq, Show)

hasChildren :: Node -> Bool
hasChildren n = (length . children) n > 0

parseNodeLine :: Parser Node
parseNodeLine = do
  name <- Data.Attoparsec.ByteString.Char8.takeWhile isAlpha_ascii
  string " ("
  weight <- decimal
  char ')'
  childNames <-
    (string " -> "
    *> (Data.Attoparsec.ByteString.Char8.takeWhile isAlpha_ascii) `sepBy` (string ", "))
    <|> pure []
  return $ Node childNames name weight

parseNodeLineFile :: Parser [Node]
parseNodeLineFile =
  many $ parseNodeLine <* endOfLine

getNodeList :: Either a [Node] -> [Node]
getNodeList (Left _) = []
getNodeList (Right ns) = ns

findRootNode :: [Node] -> Node
findRootNode [] = Empty
findRootNode nodes =
  if (null rootNode) then Empty else (head rootNode)
  where
    rootNode = filter (\x -> not (elem (name x) allChildrenNames)) nodes
    allChildrenNames = concat $ children <$> nodes

nodeNameToNode :: BS.ByteString -> [BS.ByteString] -> Parser Node
nodeNameToNode = undefined

treeFromNodeList :: Node -> [Node] -> Node
treeFromNodeList _ [] = Empty
treeFromNodeList root (ns) = undefined

testString :: BS.ByteString
testString = "ngrmq (80) -> cluej, ywrxbgi, saznyj\nfoobar (80)\nlol (80) -> jkljslkdf, jkls\n"

main = do
  inf <- BS.readFile "puzzles/puzzle07.txt"
  nodes <- return . getNodeList $ parseOnly parseNodeLineFile inf
  nodesWithChildren <- return $ filter hasChildren nodes
  rootNode <- return $ findRootNode nodesWithChildren
  print $ "Number of nodes: " ++ (show . length $ nodes)
  print $ "Number of nodes with children: " ++ (show . length $ nodesWithChildren)
  return . name . findRootNode $ nodesWithChildren
