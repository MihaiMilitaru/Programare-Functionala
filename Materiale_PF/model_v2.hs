-- Subiectul 2

getFromInterval :: Int -> Int -> [Int] -> [Int]
getFromInterval a b ls = [x | x <-ls, x>=a, x<=b]

getFromIntervalM :: Int -> Int -> [Int] -> [Int]
getFromIntervalM a b ls = do
    xs <- ls
    if xs >=a && xs <=b then return xs
    else []

-- Subiectul 1

data Point = Pt [Int]
    deriving Show

data Arb = Empty | Node Int Arb Arb
    deriving Show


class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a


insert :: Int -> Arb -> Arb
insert x Empty = Node x Empty Empty
insert x (Node y l r) 
    | x<=y = Node y (insert x l) r
    | otherwise = Node y l (insert x r) 


instance ToFromArb Point where
    fromArb Empty = Pt[]
    fromArb (Node x l r) = Pt (ll ++ [x] ++ rr)
                        where Pt ll = fromArb l
                              Pt rr = fromArb r   


    toArb (Pt[]) = Empty
    toArb (Pt x) = foldl (flip insert) Empty x


