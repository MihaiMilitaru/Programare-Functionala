data Linie = L [Int]
    deriving Show
data Matrice = M [Linie]
    deriving Show

verifica :: Matrice -> Int -> Bool
verifica (M m) n = foldr (\(L line) x -> (foldr(+) 0 line)==n && x) True m



-- pozitive :: Linie -> Int -> Bool
-- pozitive (L l) n = 
--     if length l == n 
--         then length (filter(>0) l) == n
--     else True


pozitive :: Linie -> Int -> Bool
pozitive (L l) n = 
    if length l == n 
        then foldr (\x y -> x>0 && y) True l
    else True

doarPozN :: Matrice -> Int -> Bool

doarPozN (M m) n = foldr (\(L line) x -> x && pozitive (L line) n ) True m


corect :: Matrice -> Bool

corect (M []) = True
corect (M [x]) = True
corect (M (x:y:lines)) = 
    if (length first )==(length second)
        then True && corect (M (y:lines))
        else False
    where  first = (\(L x) -> x) x
           second = (\(L y) -> y) y


---------------------------------------------------------------------------------------------------------


data Fruct
    = Mar String Bool
    | Portocala String Int


ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Mar _ _) = False
ePortocalaDeSicilia (Portocala soi felii) = if soi == "Tarocco" || soi == "Moro" || soi == "Sanguinello" then True
                                            else False

                   
nrFeliiPortocala :: Fruct -> Int
nrFeliiPortocala (Portocala _ felii) = felii
                                            
nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia [] = 0
nrFeliiSicilia (x : l)
    | ePortocalaDeSicilia(x) == True = nrFeliiPortocala(x) + nrFeliiSicilia l
    | ePortocalaDeSicilia(x) == False  = 0 + nrFeliiSicilia l
    | otherwise = 0 + nrFeliiSicilia l




nrMereViermi :: [Fruct] -> Int
nrMereViermi [] = 0
nrMereViermi ((Portocala _ _) : l) = nrMereViermi l
nrMereViermi ((Mar soi vierme) : l)
    | vierme == True = 1 + nrMereViermi(l)
    | otherwise = 0 + nrMereViermi(l)
                                            