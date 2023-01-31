import Data.Char
import Data.List
-- ex 1

nrvoc  :: [Char] ->Int
nrvoc [] = 0
nrvoc (h:t) = if h == 'a' || h == 'e' || h == 'u' || h == 'i' || h == 'o'
                then 1+nrvoc t 
                else nrvoc t 

nrVocale :: [String] -> Int

nrVocale [] = 0
nrVocale (h:t) = if reverse h == h 
    then nrvoc h + nrVocale t 
    else nrVocale t


-- ex 2

f :: Int -> [Int] -> [Int]

f _ [] = []

f x (h : t) = if even h then h : x : f x t 
                else h : f x t


-- ex 3

divizori :: Int -> [Int]

divizori x = [y | y <- [1..x], mod x y == 0 ]

-- ex 4


listadiv :: [Int] -> [[Int]]

listadiv ls = [divizori a | a <- ls]


-- ex 5

inIntervalRec :: Int -> Int -> [Int] -> [Int]

inIntervalRec a b [] = []


inIntervalRec a b (h : t) = if h>=a && h <= b then h : inIntervalRec a b t
                                                else inIntervalRec a b t


inIntervalComp :: Int -> Int -> [Int] -> [Int]

inIntervalComp a b ls = [y | y <- ls, y>=a, y<=b]


--ex 6 a) recursivitate

pozitive :: [Int] -> Int

pozitive (h:t) = if h > 0 then 1 + pozitive t
                  else pozitive t

--b) 

pozitiveComp :: [Int] -> Int

pozitiveComp ls = length[x | x <-ls , x >0]


--ex 7

--a) recursiv -> pozitiiImpareRec

pozitii :: [Int] -> Int -> [Int]
pozitii [] poz = []
pozitii (l:ls) poz = 
  if odd l 
    then poz : pozitii ls (poz + 1)
    else pozitii ls (poz + 1)

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec l = pozitii l 0


--b)descrieri de liste -> pozitiiImpareComp
pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp l = [ i | (i, x) <- zip [0..] l, odd x]



--ex 8

-- a

multDigitsRec :: String -> Int 
multDigitsRec "" = 1
multDigitsRec (xs:s) = 
  if xs `elem` "0123456789"
    then (ord xs + (-1) * ord '0' )* multDigitsRec s
    else multDigitsRec s


-- b

multDigitsComp :: String -> Int
multDigitsComp "" = 1
multDigitsComp l = product [digitToInt z | z<-l, isDigit z ]
