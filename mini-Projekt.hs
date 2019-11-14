import Data.List 
import System.IO
import Data.Function (on)

-- sort :: Ord a -> [a] -> [a]
-- group :: Eq a -> [a] -> [[a]]
-- map :: (a -> b) -> [a] -> [b]
-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]

-- recursive data type of a Binary Tree
data Tree = Leaf ([Char], Int) | Node ([Char], Int) (Tree) (Tree) deriving Show

-- Function that returns an sorted an association list with their characters and their frequency 
findFreq :: String -> [(Char, Int)]
findFreq str = map (\x -> (head x, length x)) (group (sort str))

-- Function that calls findFreq with a string 
getList = findFreq "Mississippi River"

-- Function that a Tree from Leafs
mapListToTrees :: [(Char, Int)] -> [Tree]
mapListToTrees lst =  map mapToTree lst 

-- Function that creates a Leaf from a tuple
mapToTree :: (Char, Int) -> Tree
mapToTree (c, i) =  res
                where res = Leaf ([c], i) 

-- Pattern Matching where we are interested in comparing the second elements in out Leaves                
compareTrees :: Tree -> Tree -> Ordering 
compareTrees (Leaf   (_, a))     (Node (_, b) _ _) = compare a b
compareTrees (Node (_, a) _ _) (Leaf   (_, b))     = compare a b
compareTrees (Leaf   (_, a))     (Leaf   (_, b))     = compare a b
compareTrees (Node (_, a) _ _) (Node (_, b) _ _) = compare a b

-- Function that sorts Leaves after ascending order
sortTreeList :: [Tree] -> [Tree]
sortTreeList lst = sortBy compareTrees lst

-- Functions That builds the Huffmann Tree 
buildTree :: [Tree] -> Tree
buildTree [a] = a
buildTree lst = buildTree (mergeTree (sortTreeList lst))

-- Mergers two leaves to one node, using pattern matching
mergeTree :: [Tree] -> [Tree]
mergeTree (n1@(Node (v1, f1) _ _):n2@(Node (v2, f2) _ _):rest) = (Node (v1 ++ v2, f1 + f2) n1 n2) : rest
mergeTree (n1@(Leaf (v1, f1)):n2@(Leaf (v2, f2)):rest) = (Node (v1 ++ v2, f1 + f2) n1 n2) : rest
mergeTree (n1@(Node (v1, f1) _ _):n2@(Leaf (v2, f2)):rest) = (Node (v1 ++ v2, f1 + f2) n1 n2) : rest
mergeTree (n1@(Leaf (v1, f1)):n2@(Node (v2, f2) _ _):rest) = (Node (v1 ++ v2, f1 + f2) n1 n2) : rest
                            
-- Run huffman Codeing 
run = buildTree (mapListToTrees getList)






 













 

