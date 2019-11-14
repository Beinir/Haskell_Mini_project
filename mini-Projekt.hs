--Name: Beinir Ragnuson 
--Study number: 20165800
--Email address: bragnu16@student.aau.dk

import Data.List 
import System.IO
import Data.Function (on)

-- This is only for my benefit: this gives me an idea over how the function from the prelude work, regarding their types
-- sort :: Ord a -> [a] -> [a]
-- group :: Eq a -> [a] -> [[a]]
-- map :: (a -> b) -> [a] -> [b]
-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]

-- recursive data type of a Binary Tree
data Tree = Leaf ([Char], Int) | Node ([Char], Int) (Tree) (Tree) deriving Show

-- A function that returns a sorted an association list with their characters and their frequency.
-- Here i make use of function from the prelude: Sort, group and map, which create an association list
-- that contains a char and the char's frequency
findFreq :: String -> [(Char, Int)]
findFreq str = map (\x -> (head x, length x)) (group (sort str))

-- Function that calls findFreq with the string "Mississippi River"
getList = findFreq "Mississippi River"


-- Function that maps the association list, from findFreq, into a list of trees
mapListToTrees :: [(Char, Int)] -> [Tree]
mapListToTrees lst =  map mapToTree lst 

-- Function that creates leaves from the tuples contained in the association list
-- Acts as a helper function to mapListToTrees
mapToTree :: (Char, Int) -> Tree
mapToTree (c, i) = Leaf ([c], i) 

-- Pattern Matching where we are interested in comparing the second element in our Leaves.
-- This works as a helper function for the sortTreeList function.                
compareTrees :: Tree -> Tree -> Ordering 
compareTrees (Leaf   (_, a))     (Node (_, b) _ _) = compare a b
compareTrees (Node (_, a) _ _) (Leaf   (_, b))     = compare a b
compareTrees (Leaf   (_, a))     (Leaf   (_, b))     = compare a b
compareTrees (Node (_, a) _ _) (Node (_, b) _ _) = compare a b

-- Function that sorts Leaves after ascending order
sortTreeList :: [Tree] -> [Tree]
sortTreeList lst = sortBy compareTrees lst

-- Functions That builds the Huffman Tree
-- Here we take a list of trees a parameters and use the mergeTree function to build the tree 
buildTree :: [Tree] -> Tree
buildTree [a] = a
buildTree lst = buildTree (mergeTree (sortTreeList lst))

-- Pattern Matching: Mergers two leaves to one node, using pattern matching.
-- All four instances of how a node can be merged from two leaves. 
mergeTree :: [Tree] -> [Tree]
mergeTree (n1@(Node (v1, f1) _ _):n2@(Node (v2, f2) _ _):rest) = (Node (v1 ++ v2, f1 + f2) n1 n2) : rest
mergeTree (n1@(Leaf (v1, f1)):n2@(Leaf (v2, f2)):rest) = (Node (v1 ++ v2, f1 + f2) n1 n2) : rest
mergeTree (n1@(Node (v1, f1) _ _):n2@(Leaf (v2, f2)):rest) = (Node (v1 ++ v2, f1 + f2) n1 n2) : rest
mergeTree (n1@(Leaf (v1, f1)):n2@(Node (v2, f2) _ _):rest) = (Node (v1 ++ v2, f1 + f2) n1 n2) : rest

-- Function that runs the program with the string "Mississippi River"
run = buildTree (mapListToTrees getList)
-- run the program in the terminal with an arbitrary string
-- buildTree (mapListToTrees (findFreq "Arbitrary String"))






 













 

