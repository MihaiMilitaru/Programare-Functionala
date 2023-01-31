-- ex 1

factori :: Int -> [Int]

factori x = [d | d <- [1 .. x] , mod x d == 0]

-- ex 2

prim :: Int -> Bool
prim n = length (factori n) == 2


-- ex 3

numerePrime :: Int -> [Int]
numerePrime n = [x | x<-[2..n], prim x]


-- ex 4

myzip3 :: [a] -> [a] -> [a] -> [(a,a,a)]
myzip3 x y z =  [(a, b, c) | ((a, b), c) <- zip (zip x y) z]


-- ex 5

firstEl :: [(a,b)] -> [a]

firstEl  = map fst


-- ex 6

sumList :: [[Int]] -> [Int]

sumList = map sum

-- ex 7

prel2 :: [Int] -> [Int]

prel2 = map (\x -> if even x then x `div` 2 else x *2)

-- \x -> functie anonima

-- ex 8

elem_2 :: Char -> [String] -> [Bool]

elem_2 n = map (elem n)


elem_3 :: Char -> [[Char]] -> [[Char]]

elem_3 n s = filter (elem n) s


-- ex 9

patrate :: [Int] -> [Int]

patrate ls = map (^2) (filter odd ls)

-- ex 10

patrate_impare :: [Int] -> [Int]

patrate_impare v = map ((^2) . fst)(filter (even .snd ) (zip v [1 .. length v]))



-- ex 11

vocale :: [Char] -> [Char]
vocale "" = ""
vocale (h:t) = if h `elem` "aeiouAEIOU" then h : vocale t else vocale t

eliminare :: [[Char]] -> [[Char]]
eliminare = map vocale

-- ex 12

mymap :: (a->b) -> [a] -> [b]
mymap f [] = []
mymap f (h:t) = f h : mymap f t

myfilter :: (a->Bool) -> [a] -> [a]
myfilter f []= []
myfilter f (h:t) = if f h then h:myfilter f t else myfilter f t



