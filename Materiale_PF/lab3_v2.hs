import Data.Char
import Data.List

nrvoc :: String -> Int
nrvoc [] = 0
nrvoc (h:t) = if h == 'a' || h == 'e' || h == 'i' || h == 'o' || h == 'u' then 1 + nrvoc t
                else nrvoc t


nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (h:t) = if reverse h == h then nrvoc h + nrVocale t
                    else nrVocale t

addeven :: Int -> [Int] -> [Int]
addeven a [] = []
addeven a (x:xs) = if even x then x : a : addeven a xs
                    else x : addeven a xs


-- varianta cu do
addEvenM :: Int -> [Int] -> [Int]
addEvenM a ls = do
    x <- ls
    if even x then return x
    else return a


divizori :: Int -> [Int] 
divizori n = [x | x<-[1..n], n `mod` x ==0 ]

-- varianta cu do
divizoriM :: Int -> [Int]
divizoriM n = do
    x <- [1..n]
    if n `mod` x == 0 then return x
    else []


listadiv :: [Int] -> [[Int]]
listadiv [] = []
listadiv (x:xs)=divizori x : listadiv xs

-- varianta cu do
listaDivM :: [Int] -> [[Int]]
listaDivM ls = do
    x <- ls
    return (divizori x)




inInterval :: Int -> Int -> [Int] -> [Int]
inInterval a b ls = [y | y<-ls, y>=a, y<=b]

-- varianta cu do
inIntervalM :: Int -> Int -> [Int] -> [Int]
inIntervalM a b ls = do
    x <- ls
    if x>=a && x<=b then return x
    else []



inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec a b [] = [] 
inIntervalRec a b (x:xs) = if x>=a && x<=b then x:inIntervalRec a b xs
                            else inIntervalRec a b xs


pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (x:xs) = if x>=0 then 1 + pozitiveRec xs
                        else pozitiveRec xs



pozitiveComp :: [Int] -> Int
pozitiveComp ls = length [x | x <-ls, x>=0]


pozitii :: [Int] -> Int -> [Int]
pozitii [] _ = []
pozitii (x:xs) a = if odd x then a : pozitii xs (a+1)
                    else pozitii xs (a+1)



pozitiiImpare :: [Int] -> [Int]
pozitiiImpare ls = pozitii ls 0


pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp l = [i | (i, x) <- zip [1..] l , odd x]

multDigits :: String -> Int
multDigits [] = 1
multDigits (h:t) = if h `elem` "0123456789" then (ord h - 1 * ord '0') * multDigits t
                    else multDigits t

multDigitsComp :: String -> Int
multDigitsComp l = product [digitToInt z | z<-l, isDigit z]
