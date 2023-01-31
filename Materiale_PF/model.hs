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
    | x <= y = Node y (insert x l) r
    | otherwise = Node y l (insert x r)



instance ToFromArb Point where
    toArb (Pt []) = Empty
    toArb (Pt (x:xs)) = foldl (flip insert) (Node x Empty Empty) xs

    fromArb Empty = Pt []
    fromArb (Node x l r) = Pt (ll  ++ [x] ++ rr)
        where Pt ll = fromArb l
              Pt rr = fromArb r





---------------------------------------------

getFromInterval :: Int -> Int -> [Int] -> [Int]
getFromInterval x y ls = [nr | nr <- ls, x <= nr && nr <= y]


getFromIntervalMonad :: Int -> Int -> [Int] -> [Int]
getFromIntervalMonad x y ls = do 
                            nr <- ls
                            if x <= nr && nr <= y then
                                return nr
                            else []

