



-- exercitii

--1
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

poly2 :: Double -> Double -> Double -> Double ->Double
poly2 a b c x = a * x^2 + b*x + c

--2

eeny :: Integer -> String

eeny x = if mod x 2 == 0 then "eeny"
        else "meeny"


--3 
fizzbuzz :: Integer -> String
fizzbuzz x 
        | mod x 5 ==0 && mod x 3 == 0 = "FizzBuzz"
        | mod x 5 ==0 = "Buzz"
        | mod x 3 ==0 = "Fizz"

--4
tribonacci :: Integer -> Integer

tribonacci n
        | n==1 = 1
        | n==2 = 1
        | n==3 = 2
        | otherwise = tribonacci (n-1) + tribonacci(n-2) + tribonacci(n-3)


--5

binomial :: Integer -> Integer -> Integer
binomial n k 
        | k==0 = 1
        | n==0 = 1
        | otherwise = binomial (n-1) k + binomial (n-1) (k-1)

--6 a

verifL :: [Int] -> Bool

verifL x = if mod (length x) 2 ==0 then True else False

--6 b

takefinal :: [Int] -> Int -> [Int]

takefinal x n = if length x <= n then x
                else takefinal (tail x) n


--6 c 
remove :: [Int] -> Integer -> [Int]

remove x n = if n /=0 then [head x] ++ remove (tail x) (n-1) else tail x

--7 a

myreplicate :: Int -> Int -> [Int]

myreplicate n a = [a | x <- [1..n]]

myreplicate n a 
            | n==0 = []
            | otherwise = [a] ++ myreplicate(n-1) a 

--7 b

sumImp :: [Int] -> Int

sumImp s = sum [x | x <-s, mod x 2 == 1]

-- 7 c

totalLen :: [String] -> Int

totalLen s = sum[length x | x<-s, head x == 'A']