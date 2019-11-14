import Data.List 
import System.IO
import Data.Function (on)

-- sort :: Ord a -> [a] -> [a]
-- group :: Eq a -> [a] -> [[a]]
-- map :: (a -> b) -> [a] -> [b]
-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]

-- Binary Tree
data Btree = BLeaf ([Char], Int) | BBranch ([Char], Int) (Btree) (Btree) deriving Show

-- Function that returns an sorted association list with characters and their frequency 
findFreq :: String -> [(Char, Int)]
findFreq str = map (\x -> (head x, length x)) (group (sort str))

-- Function that calls findFreq with a string 
getList = findFreq "Mississippi River"

-- Function that a Btree from BLeafs
mapListToTrees :: [(Char, Int)] -> [Btree]
mapListToTrees lst =  map mapToTree lst 

-- Function that creates a BLeaf from a tuple
mapToTree :: (Char, Int) -> Btree
mapToTree (c, i) =  res
                where res = BLeaf ([c], i) 

-- Pattern Matching where we are interested in comparing the second elements in out BLeafs                
compareTrees :: Btree -> Btree -> Ordering 
compareTrees (BLeaf   (_, a))     (BBranch (_, b) _ _) = compare a b
compareTrees (BBranch (_, a) _ _) (BLeaf   (_, b))     = compare a b
compareTrees (BLeaf   (_, a))     (BLeaf   (_, b))     = compare a b
compareTrees (BBranch (_, a) _ _) (BBranch (_, b) _ _) = compare a b

-- Function that sorts BLeafs after ascending order
sortTreeList :: [Btree] -> [Btree]
sortTreeList lst = sortBy compareTrees lst

-- 
getElements :: [Btree] -> [Btree]
getElements (n1@(BBranch (v1, f1) _ _):n2@(BBranch (v2, f2) _ _):rest) = (BBranch (v1 ++ v2, f1 + f2) n1 n2) : rest
getElements (n1@(BLeaf (v1, f1)):n2@(BLeaf (v2, f2)):rest) = (BBranch (v1 ++ v2, f1 + f2) n1 n2) : rest
getElements (n1@(BBranch (v1, f1) _ _):n2@(BLeaf (v2, f2)):rest) = (BBranch (v1 ++ v2, f1 + f2) n1 n2) : rest
getElements (n1@(BLeaf (v1, f1)):n2@(BBranch (v2, f2) _ _):rest) = (BBranch (v1 ++ v2, f1 + f2) n1 n2) : rest


-- Functions That builds the Tree 
buildTree :: [Btree] -> Btree
buildTree [a] = a
buildTree lst = buildTree (getElements (sortTreeList lst))
                                                            
----------------------------------------------------------------------------------------

-- Run
--run = sortTreeList (mapListToTrees getList)
--run = mapListToTrees getList 
--sortTreeList (mapListToTrees getList) 

run1 = sortTreeList (mapListToTrees getList)
--run2 = getElements getList
run3 = buildTree (mapListToTrees getList)
--run3 = sortTreeList (mapListToTrees getElements getList)
--run4 = getElements (mapListToTrees getList)





 













 

