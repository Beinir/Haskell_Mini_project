import Data.List 
import System.IO
import Data.Function (on)

treeList = [('a', 1),('b', 2), ('c', 3), ('d', 4) ]

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

createTree :: [a] -> Tree a
createTree [] = Leaf
foldTree xs = Node height 
            (foldTree (take half xs)) 
        (xs !! half)
            (foldTree $ drop (half + 1) xs)
        where 
            len = length xs
            half = len `div` 2
            height = floor $ logBase 2 (fromIntegral len)



--Binary Tree
data Btree = BLeaf Char Int | BBranch (Btree, Btree, Int) deriving Show

--mergeNodes :: ([Char], Int) -> ([Char], Int) -> ([Char], Int) -> Btree
--mergeNodes node left right = BBranch (node, BLeaf left, BLeaf right)

getCount (BLeaf (_ ,w)) = w 
getCount (BBranch (_,_,w)) = w
merge t1 t2 = BBranch (t1,t2, (getCount t1 + getCount t2 ))

--treeList = [('a', 1),('b', 2), ('c', 3), ('d', 4) ]

--someTree = BBranch(BLeaf ('a', 7), BLeaf ('a', 7), 9 )

genTree (x: []) = x
genTree (x:y:xs) = genTree (insertBy (compare `on` getCount) (merge x y) xs)






