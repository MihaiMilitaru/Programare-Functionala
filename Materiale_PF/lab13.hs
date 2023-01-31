pos :: Int -> Bool
pos x = if (x>=0) then True else False

-- fct :: Maybe Int -> Maybe Bool
-- fct mx = mx >>= (\x -> Just (pos x))

fct mx = do
    x <- mx
    return (pos x)


-------------------------------------------------------------------------------

addM :: Maybe Int -> Maybe Int -> Maybe Int

addM mx my = do
    x <- mx
    y <- my
    return(x+y)
    
cartesian :: Monad m => p -> m a -> m b -> m(a, b)
cartesian product xs ys = do
    x <- xs
    y <- ys
    return (x,y)

prod:: Monad m => (t1 -> t2 -> b) -> m t1 -> m t2 -> m b
prod f xs ys = do
    x <- xs
    y <- ys
    return(f x y)


mGetLine :: IO String
mGetLine = getChar >>= \x ->
    if x == '\n' then
        return []
    else 
        mGetLine >>= \xs -> return (x:xs)

prelNo noin = sqrt noin

ioNumberSecv=
    (readLn::IO Float)>>= \noin -> 
        putStrLn("Intrare\n"++show noin)>>= \_-> 
            let noout = prelNo noin in
                putStrLn "Iesire" >>= \_ -> print $ prelNo noin



--------------------------------------------------------------------------------------

newtype WriterS a = Writer { runWriter :: (a, String) } 
    deriving Show

instance  Monad WriterS where
  return va = Writer (va, "")
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)


instance  Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterS where              
  fmap f ma = pure f <*> ma     



tell :: String -> WriterS () 
tell log = Writer ((), log)

logIncrement :: Int  -> WriterS Int
logIncrement x = Writer (x+1, "Incremented " ++ show x ++ " to " ++ show (x+1) ++ " ")

logIncrementN :: Int -> Int -> WriterS Int
logIncrementN n x = do
    tell ("Incrementing " ++ show x ++ " " ++ show n ++ " times")
    let x' = x + n
    tell ("Result: " ++ show x')
    return x'

myGetLine :: IO String
myGetLine = getChar >>= \x ->
    if x == '\n' then
    return []
    else
    myGetLine >>= \xs -> return (x:xs)




