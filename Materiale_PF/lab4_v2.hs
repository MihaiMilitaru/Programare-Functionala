factori :: Int -> [Int]
factori n = [x| x<-[1..n], mod n x == 0 ]


prim :: Int -> Bool
prim n = if length (factori n) == 2 then True
          else False
          

numerePrime :: Int -> [Int]
numerePrime n = [x | x<-[2..n], prim x == True]


myzip3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip3 x y z = [(a, b, c)| ((a,b), c) <- zip(zip x y) z]


firstEl :: [(a, b)] -> [a]
firstEl ls = [fst x| x<- ls]

-- varianta cu do

firstElM :: [(a, b)] -> [a]
firstElM ls = do
    x <- ls
    return (fst x)




sumList :: [[Int]] -> [Int]
sumList [] = []
sumList (x:xs) = sum x : sumList xs

-- varianta cu do

sumListM :: [[Int]] -> [Int]
sumListM ls = do
    x <- ls
    return (sum x)

process :: Int -> Int
process x = if even x then x `div` 2
            else x*2

prel2 :: [Int] -> [Int]
prel2 ls = [process x | x<- ls]

-- varianta cu do

prel2M :: [Int] -> [Int]
prel2M ls = do
    x <- ls
    return (process x)



checkchar :: Char -> [String] -> [String]
checkchar c s = filter (elem c) s

-- varianta cu do

checkcharM :: Char -> [String] -> [String]
checkcharM c s = do
    x <- s
    if elem c x then return x
    else []



checkchar2 :: Char -> [String] -> [String]
checkchar2 _ [] = []
checkchar2 c (x:xs) = if elem c x == True then x : checkchar2 c xs
                        else checkchar2 c xs



patrate :: [Int] -> [Int]
patrate ls = map (^2) (filter odd ls)

-- varianta cu do

patrateM :: [Int] -> [Int]
patrateM ls = do
    x <- ls
    if odd x then return (x^2)
    else []




patrateImp :: [Int] -> [Int]
patrateImp ls = map ((^2) . fst)(filter (even . snd)(zip ls [0..]))

-- varianta cu do

patrateImpM :: [Int] -> [Int]
patrateImpM ls = do
    x <- zip ls [0..]
    if even (snd x) then return ((fst x)^2)
    else []


eliminarec :: String -> String
eliminarec "" = ""
eliminarec (h:t) = if (h `elem` "aeiou") == True then h : eliminarec t
                    else eliminarec t


-- varianta cu do

eliminarecM :: String -> String
eliminarecM s = do
    x <- s
    if (x `elem` "aeiou") == True then return x
    else []

eliminareC :: [String] -> [String]
eliminareC ls = map eliminarec ls

-- varianta cu do

eliminareCM :: [String] -> [String]
eliminareCM ls = do
    x <- ls
    return (eliminarec x)



mymap :: (a->b) -> [a] -> [b]
mymap f [] = []
mymap f (h:t)= f h : mymap f t

myfilter :: (a->Bool) -> [a] -> [a]
myfilter f (h:t)= if f h   then h : myfilter f t
                    else myfilter f t