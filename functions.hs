-- Haskell functions 
import Data.List 
import System.IO
import Data.Function (on)

-- Adds two numbers of type in together 
addMe :: Int -> Int -> Int 

addMe x y = x + y

-- Adds two tuples of type in together

addTuples :: ([Char], Int) -> ([Char], Int) -> ([Char], Int)

addTuples (x1, y1) (x2, y2) = (x1 ++ x2 , y1 + y2)

-- int as input and string as out put 

whatAge :: Int -> String

whatAge 16 = "you can't drink"
whatAge 18 = "you can drink"
whatAge 30 = "Pipar"
whatAge x = "Noting important"

-- Factorial functions 
fac :: Int -> Int 
fac2 :: Int -> Int

fac 0 = 1
fac n = n * fac(n-1)
fac2 n = product [1..n]

-- Guard function
bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  


isOdd :: Int -> Bool
isOdd n 
    | n `mod` 2  == 0 = False 
    | otherwise = True

isEven n = n `mod` 2 == 0

whatGrade :: Int -> String

whatGrade age 
    | (age >= 5) && (age <= 6) = "Kindergarder"
    | (age >= 8) && (age <= 15) = "Elementary Schools"
    | otherwise = "Working Class Hero"

-- Map functions 

square x = x * x 

map'  = map square [1,2,3,4]
    

-- Where clause and guards 

avgScore :: Double -> Double -> String
avgScore games goals 
    | avg <= 0.2 = ": Not good"
    | avg <= 0.5 = ": preaty good"
    | avg <= 0.8 = ": Very good"
    | avg <= 1 =  ": Exceptional"
    | avg > 1 =  ":GOAT"
    where avg = goals/games 

-- Access list items

getListItems :: [Int] -> String

getListItems [] = "List is empty"
getListItems (x:xs) = "1st is " ++ show x ++ " and the rest are" ++ show xs 

getFirstItem :: String -> String
getFirstItem [] = "Empty"
getFirstItem all@(x:xs) = [x]

-- Higher ordred functions: passing functions as values 
times4 :: Int -> Int
times4 x = x * 4
listTimes4 = map times4 [1,2,3,4]

multBy4 :: [Int] -> [Int]
multBy4 [] = []
multBy4 (x:xs) = times4 x : multBy4 xs 

-- Recursion 

areStringEq :: [Char] -> [Char] -> Bool
areStringEq [][] = True 
areStringEq (x:xs) (y:ys) = x == y && areStringEq xs ys
areStringEq _ _ = False


-- Pass a function into a function 

-- Receive  
doMult :: (Int -> Int) -> Int
doMult func = func 3
num3Times4 = doMult times4

-- Return 
getAddFunc :: Int -> (Int -> Int)
getAddFunc x y = x + y
adds3 = getAddFunc 3
fourPlus3 = adds3 4

-- Lambda
addInsideList = map (\x -> x * 2) [1..10]


-- conditionals

doubleEvenNumber :: Int -> Int
doubleEvenNumber x = 
    if (x `mod` 2 /= 0) then x 
        else x * 2
    
getClass :: Int -> String
getClass n = case n of
    5 -> "Is not 6"
    6 -> "Is not 5"
    _ -> "You make now sense" 
    

-- Enumerated Types

data Fotballer = GK | DF | LB | RB | DM | AM | CM | RW | LW | ST deriving Show 

ozil :: Fotballer -> Bool
ozil AM = True 
ozilAM = print(ozil AM)

-- Custom Types 

data Customer = Customer String String Int deriving Show

tomSmith :: Customer

tomSmith = Customer "Tom" "Smith" 29

getAge :: Customer -> Int

getAge (Customer _ _ a) = a

-- Rock paper scissors 

data RPS = Rock | Paper | Scissors deriving Show

shoot :: RPS -> RPS -> String
shoot Paper Rock = "Paper Wins"
shoot Paper Scissors = "Scissors Wins"
--shoot Rock paper = "Paper Wins"
shoot Rock scissors = "Rock Wins"
--shoot Scissors Rock = "Rock Wins"
--shoot Scissors paper = "Scissors Wins"
shoot _ _ = "Error"

-- Two versions of a type

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving Show

area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2 
area (Rectangle x y x2 y2) = (abs (x2 - x)) * (abs (y2 -y)) 

circleArea = area(Circle 1.2 1.2 1.2)
rectangleArea = area(Rectangle 5.5 7.2 5.5 3.2)

-- Dot operator 
sumValue1 = putStrLn (show (1 + 2))
sumValue2 = putStrLn . show $ 1 + 2

-- Type Classes 


-- Fib List 
fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib) ]

fib300 = fib !! 300
fibTake = take 10 fib


doList = sortBy (compare `on` snd) [('a', 5) ,('z', 4), ('b', 2), ('a', 3)]













