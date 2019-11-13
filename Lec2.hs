data BExp = TT | FF | Or BExp BExp | And BExp BExp | Neg BExp

eval :: BExp -> Bool

eval TT = True 
eval FF = False 

eval (Or b1 b2) = tv1 || tv2 
  where tv1 = eval b1 
        tv2 = eval b2

eval (And b1 b2) = tv1 && tv2 
  where tv1 = eval b1 
        tv2 = eval b2


eval (Neg b1) = not tv 
  where tv = eval b1


squares n = [x*x | x <- [1..n]]

isPrime k = if k > 1 then null 
  [ x | x <- [2..k - 1], k `mod` x == 0] 
    else False

addThis a b = a + b 

fac n = n * fac(n - 1)

