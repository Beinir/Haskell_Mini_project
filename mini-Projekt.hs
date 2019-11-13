import Data.List 
import System.IO
import Data.Function (on)

-- sort :: Ord a -> [a] -> [a]
-- group :: Eq a -> [a] -> [[a]]
-- map :: (a -> b) -> [a] -> [b]
-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]


-- Function that returns an sorted association list with characters and their frequency 
findFreq :: String -> [(Char, Int)]
findFreq str =  sortBy (compare `on` snd) (map (\x -> (head x, length x)) ( group (sort str)))

--  Function that calls findFreq with a string 
getList = findFreq "Mississippi River"

-- gets elements in the tupels 
getElements :: [(Char, Int)] -> Btree
getElements (x:xs) = let first = (x:xs)!!1
                         second = (x:xs)!!2 
                        in conElements first second
  
conElements :: (Char, Int) -> (Char, Int) -> Btree
conElements (x1, y1) (x2, y2) = let node =  ([x1, x2], y1 + y2)
                                    left = ([x1], y1)
                                    right = ([x2], y2)
                                    in mergeNodes node left right
                                  
-- Binary Tree
data Btree = BLeaf ([Char], Int) | BBranch (([Char], Int), Btree, Btree) deriving Show

mergeNodes :: ([Char], Int) -> ([Char], Int) -> ([Char], Int) -> Btree
mergeNodes node left right = BBranch (node, BLeaf left, BLeaf right)

-- Run
run = getElements getList 


 













 

