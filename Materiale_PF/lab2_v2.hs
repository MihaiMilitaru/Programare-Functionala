poly2 :: Double -> Double -> Double -> Double -> Double
poly2 a b c x = a*x^2 + b*x + c

eeny :: Int -> String
eeny n = if even n then "eeny" else "meeny"

fizzbuzz :: Integer -> String
fizzbuzz n 
    | n `mod` 15 == 0 = "fizzbuzz"
    | n `mod` 3 == 0 = "fizz"
    | n `mod` 5 == 0 = "buzz"
    | otherwise = show n


tribonacci :: Integer -> Integer
tribonacci n
    | n == 0 = 0
    | n == 1 = 0
    | n == 2 = 1
    | otherwise = tribonacci (n-1) + tribonacci (n-2) + tribonacci (n-3)


binomial :: Integer -> Integer -> Integer
binomial n k
    | k == 0 = 1
    | n == 0 = 0
    | otherwise = binomial (n-1) (k-1) + binomial (n-1) k


verifL :: [Int] -> Bool
verifL [] = True
verifL x = if even (length x) then True else False 


takefinal :: [Int] -> Int -> [Int]
takefinal x n = if length x <=n then x
                else takefinal (tail x) n

takefinalstring :: [String] -> Int -> [String]
takefinalstring x n = if length x <=n then x
                else takefinalstring (tail x) n

remove :: [Int] -> Int -> [Int]
remove x n = if n /=0 then head x : remove (tail x) (n-1) else tail x


myreplicate :: Int -> Int -> [Int]
myreplicate n x = if n /=0 then x : myreplicate (n-1) x else []

sumImp :: [Int] -> Int
sumImp x = if even (head x) then head x + sumImp (tail x) else sumImp (tail x)


totalLen :: [String] -> Int
totalLen s = sum [length x| x<- s , head x == 'A']

