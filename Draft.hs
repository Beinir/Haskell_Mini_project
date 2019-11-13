import Data.List 
import System.IO
import Data.Function (on)

 

-- Function that returns an association list with characters and their frequency 
findFreq :: String -> [(Char, Int)]
findFreq str =  sortBy (compare `on` snd) (map (\x -> (head x, length x)) ( group (sort str)))

--  Function that calls findFreq with a string 
getList = findFreq "Mississippi River"

-- gets elements in the tupels 
--getElements :: [(Char, Int)] -> ([Char], Int)
getElements (x:xs) = let first = (x:xs)!!1
                         second = (x:xs)!!2 
                        in conElements first second
  
--conElements :: (Char, Int) -> (Char, Int) -> (([Char], Int) -> ([Char], Int) -> ([Char], Int))
conElements (x1, y1) (x2, y2) =  let node =  ([x1, x2], y1 + y2)
                                     left = ([x1], y1)
                                     right = ([x2], y2)
                                    in node

-- intermediate 
                                    

-- Binary Tree
data Btree = BLeaf (Char, Int) | BBranch (Btree, Btree, Int) deriving Show

--mergeNodes :: ([Char], Int) -> ([Char], Int) -> ([Char], Int) -> Btree
--mergeNodes node left right = BBranch (node, BLeaf left, BLeaf right)

getCount (BLeaf (_ ,w)) = w 
getCount (BBranch (_,_,w)) = w
merge t1 t2 = BBranch (t1,t2, (getCount t1 + getCount t2 ))

treeList = [('a', 1),('b', 2), ('c', 3), ('d', 4) ]

genTree (x: []) = x
genTree (x:y:xs) = genTree (insertBy (compare `on` getCount) (merge x y) xs)

-- Run
run =  getElements getList 



 













 

