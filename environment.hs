-- Haskell Environment 

-- Exercise 1.1: fibonacci function  
-- fib :: (Num t) => t -> t
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
-- Time Complexety of fib(n) is O(fib(n))

-- Exercise 1.2: reverses a list 
-- list_reverse [t] -> [t], more specifically list_reverse [int] -> [int]
a =[1,2,3,4,5,6] 
list_reverse [] = []
list_reverse (x:xs) = (list_reverse xs) ++ [x]
-- ++ is a concatenate function 


-- Concatenates lists aa and bb
aa = [1, 2, 3, 4, 5, 6]
bb = [1, 2, 3, 4, 5, 6]
con_this a b = aa ++ bb

-- Exercise 1.3: palindrome function
-- ispalindrome :: (Eq t) => [t] -> Bool
ispalindrome l = (l == list_reverse l)

-- Exercise 2.1: isprime 
isprime k = if k > 1 then null [ x | x <- [2..k - 1], k `mod` x == 0] else False














