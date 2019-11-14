import Data.List 
import System.IO
import Data.Function (on)

--treeList = [('a', 1),('b', 2), ('c', 3), ('d', 4) ]

--data Tree a = Leaf | Node Int (Tree a) a (Tree a) deriving Show

--createTree :: [a] -> Tree a
--createTree [] = Leaf
--createTree xs = Node height 
--            (createTree (take half xs)) 
--        (xs !! half)
--            (createTree (drop (half + 1) xs))
--        where 
--            len = length xs
--            half = len `div` 2
--            height = floor ( logBase 2 (fromIntegral len))



--Binary Tree
--data Tree = BLeaf Char Int | BBranch(Tree, Tree, Int) deriving Show 

--mergeNodes :: ([Char], Int) -> ([Char], Int) -> ([Char], Int) -> Btree
--mergeNodes node left right = BBranch (node, BLeaf left, BLeaf right)

--getCount (BLeaf (_ , w)) = w 
--getCount (BBranch (_ , _ , w)) = w
--merge t1 t2 = BBranch (t1,t2, (getCount t1 + getCount t2 ))

--treeList = [('a', 1),('b', 2), ('c', 3), ('d', 4) ]

--someTree = BBranch(BLeaf ('a', 7), BLeaf ('a', 7), 9 )

--genTree (x: []) = x
--genTree (x:y:xs) = genTree (insertBy (compare `on` getCount) (merge x y) xs)






------------------------------------------------------------------------------------------
conElements :: (Char, Int) -> (Char, Int) -> Btree
conElements (x1, y1) (x2, y2) = (mergeNodes node left right)
                            where node =  ([x1, x2], y1 + y2)
                                 left = ([x1], y1)
                                  right = ([x2], y2)
                                  
--mergeNodes :: ([Char], Int) -> ([Char], Int) -> ([Char], Int) -> Btree
--mergeNodes first second = BBranch left right 


-----------------------------------------------------------------------------------------
-- Important 

--gets elements in the tupels 
--getElements :: [Btree] -> Btree
getElements (first:second:rest) =   --buildTree(first second):rest 

buildTree :: [Btree] -> Btree
buildTree [a] = a
buildTree lst = buildTree (getElements  (sortTreeList lst))


                              

----------------------------------------------------------------------------------------

-- Run
--run = sortTreeList (mapListToTrees getList)
--run = mapListToTrees getList 
--sortTreeList (mapListToTrees getList) 

run1 = sortTreeList (mapListToTrees getList)
run2 = getElements getList
run3 = buildTree (mapListToTrees getList)
--run3 = (sortTreeList(getElements (sortTreeList (mapListToTrees getList)))
--run3 = sortTreeList (mapListToTrees getElements getList)
--run4 = getElements (mapListToTrees getList)