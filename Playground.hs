import Data.List
import System.IO


-- Haskell playground

--Concatenates two lists 
n = [1,2,3,4]
conList n = n ++ [5,6,7,8]

-- Sums the elements in a list 
sumOf = sum [1..10]


-- Association list 
assList = [(1, False), (2, True), (3, False), (4, True)]

-- Check length 
check n = if (length assList > 0) then True else False 

-- Access intex in list 
this_list1 = [1,2,3,4,5]

getElm lst = lst !! 3

getFirst lst = last lst

getNotLast lst = init lst

get3First lst = take 3 lst

getNot3First lst = drop 3 lst

-- Is elm in list 
isInList n lst = n `elem` lst 

-- Max and min value in list 
maxValue lst = maximum lst
minValue lst = minimum lst

-- Operation on all values in a list 
-- Multiply every element in list with 10

listTime2 = [x * 2 | x <- [1..8]]

-- Same but with filtering 
listTimeFilter = [x * 10 | x <- [1..8], x * 10 <= 50 ]

-- Filtering 
this_list2 = [1,29,32,29,5,29,7,8,9,10]
beinirAge = 29
elmCondition lst = [x | x <- lst, x == beinirAge ] 

-- Sort 
sortList = sort [1,4,6,5,3,4,7,9,7,5]












