-- ex1

sumImpare :: [Int] -> Int
sumImpare l = foldl (+) 0  ( map(^2)( filter (odd) l))

-- ex2

verif :: [Bool] -> Bool
verif l = foldr (&&) True l

-- ex3

allVerifies :: (Int -> Bool) -> [Int] -> Bool

allVerifies f l = foldr (&&) True (map f l)


-- ex 4

anyVerifies :: (Int -> Bool) -> [Int] -> Bool

anyVerifies f l = foldr (||) False (map f l)

-- ex 5

mapFoldr1 :: (a -> b) -> [a] -> [b]
mapFoldr1 f l = foldr (:) [] [f x | x<-l]

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr cond = foldr (\x xs -> cond x : xs) []

filterFoldr :: (a->Bool) -> [a] -> [a]
filterFoldr f l = foldr (:) [] [x | x<-l, f x ==True]

-- ex 6

listToInt :: [Integer] -> Integer

listToInt l = read(foldl (++) "" [show x | x<-l])

-- ex 7

-- a)

rmChar :: Char -> String -> String

rmChar chr = filter (\x -> x > chr || x < chr)

-- b)

rmCharsRec :: String -> String -> String

rmCharsRec (h:t) str2
  | t == "" = val
  | otherwise = rmCharsRec t val
  where val = rmChar h str2


  -- c)

 
rmCharsFold :: String -> String -> String
rmCharsFold s1 s2 = foldr rmChar s2 s1








