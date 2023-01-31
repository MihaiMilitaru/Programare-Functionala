data Expr = Const Int -- integer constant
    | Expr :+: Expr -- addition
    | Expr :*: Expr -- multiplication
    deriving Eq

data Operation = Add | Mult deriving (Eq, Show)


data Tree = Lf Int -- leaf

    | Node Operation Tree Tree -- branch
    deriving (Eq, Show)


instance Show Expr where
    show :: Expr -> String
    show (Const i) = show i
    show (e1 :+: e2)= "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
    show (e1 :*: e2)= "(" ++ show e1 ++ "*" ++ show e2 ++ ")"


evalExp :: Expr -> Int
evalExp (Const x)= x
evalExp (x1 :+: x2) = (evalExp x1) + (evalExp x2)
evalexp (x1 :*: x2) = (evalExp x1) * (evalExp x2)

evalArb :: Tree -> Int
evalArb (Lf x) = x
evalArb(Node Add x1 x2)=(evalArb x1)+(evalArb x2)
evalArb(Node Mult x1 x2)=(evalArb x1)*(evalArb x2)

expToArb :: Expr -> Tree
expToArb(Const x)= Lf x 
expToArb(x1 :+: x2)= Node Add(expToArb x1)(expToArb x2)
expToArb(x1 :*: x2)= Node Mult(expToArb x1)(expToArb x2)



class Collection c where
    empty :: c key value
    singleton :: key -> value -> c key value
    insert
        :: Ord key
        => key -> value -> c key value -> c key value
    clookup :: Ord key => key -> c key value -> Maybe value
    delete :: Ord key => key -> c key value -> c key value
    keys :: c key value -> [key]
    values :: c key value -> [value]
    toList :: c key value -> [(key, value)]
    fromList :: Ord key => [(key,value)] -> c key value


    keys c = [fst x| x <- toList c]
    values c = [snd x| x<- toList c]
    fromList [] = empty
    fromList ((k,v):kvs) = insert k v (fromList kvs)
    

newtype PairList k v
    = PairList { getPairList :: [(k, v)] }

instance Collection PairList  where
    empty = PairList []
    singleton k v = PairList [(k, v)]

    insert k v (PairList kvs) = PairList ((k, v) : kvs)

    clookup k (PairList kvs) = lookup k kvs
    clookup k p = lookup k (getPairList p)

    delete k (PairList kvs) = PairList (filter(\(kk, _) -> kk /= k) kvs)

    toList = getPairList

----------------------------------------------------------------------------------------------
data SearchTree key value
    = Empty
    | BNode
        (SearchTree key value) -- elemente cu cheia mai mica
        key -- cheia elementului
        (Maybe value) -- valoarea elementului
        (SearchTree key value) -- elemente cu cheia mai mare


instance Collection SearchTree where

    empty = Empty
    singleton k v = BNode Empty k (Just v) Empty

    insert k v Empty = singleton k v

    insert k v (BNode l key val r)
        | k < key = BNode (insert k v l) key val r
        | k > key = BNode l key val (insert k v r)
        | k == key = BNode l key (Just v) r

    clookup _ Empty = Nothing

    clookup k (BNode l key value r )
        | k<key = clookup k l
        | k>key = clookup k r
        | k==key = value
        
    delete k (BNode l key value r)
        | k<key = BNode (delete k l) key value r
        | k>key = BNode l key value (delete k r)
        | k==key = BNode l key Nothing r

    toList Empty = []
    toList (BNode l key (Just value) r ) = toList l ++ [(key, value)] ++ toList r 
    toList (BNode l _ Nothing r ) = toList l++ toList r


        
