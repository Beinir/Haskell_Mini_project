
fac 0 = 1
fac n = n * fac (n - 1)

-- Problem 1

-- fib :: (Num t) => t -> t

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- The time complexity of computing fib(n) is O(fib(n))