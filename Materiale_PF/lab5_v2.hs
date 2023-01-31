sumImpare :: [Int] -> Int
sumImpare ls = foldl (+) 0 (map(^2) (filter odd ls))

verif :: [Bool] -> Bool
verif ls = foldr (&&) True ls

allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies f ls = foldr (&&) True (map f ls)

anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies f ls = foldr (||) False (map f ls)


mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f ls = foldr (:) [] [f x | x <- ls]

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr f ls = foldr (:) [] [x | x<-ls, f x == True]


listToInt :: [Integer] -> Integer
listToInt ls = read(foldl (++) "" [show x| x<-ls])


rmChar :: Char -> String -> String
rmChar _ "" = ""
rmChar c (h:t) = if h /= c then h : rmChar c t
                    else rmChar c t 


  
rmCharsRec :: String -> String -> String
rmCharsRec (h:t) str2 
    | t == "" = rmChar h str2
    | otherwise = rmCharsRec t (rmChar h str2)


rmCharsFold :: String -> String -> String
rmCharsFold s1 s2 = foldr rmChar s2 s1

