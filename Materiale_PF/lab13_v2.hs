pos :: Int -> Bool
pos x = if (x>=0) then True else False



-- fct :: Maybe Int -> Maybe Bool
-- fct mx = mx >>= (\x -> Just (pos x))


fct :: Maybe Int -> Maybe Bool
fct mx = do
    x <- mx
    return (pos x)


    
addM :: Maybe Int -> Maybe Int -> Maybe Int
addM mx my = mx >>= \x -> my >>= \y -> return (x+y)

addM2 :: Maybe Int -> Maybe Int -> Maybe Int
addM2 mx my = do
    x <- mx
    y <- my
    return (x+y)

addM3 :: Maybe Int -> Maybe Int -> Maybe Int
addM3 mx my = Just (x+y)
    where
        Just x = mx
        Just y = my


cartesian_product xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))
cartesian_product2 xs ys = do
    x <- xs
    y <- ys
    return (x,y)

prod f xs ys = [f x y | x <- xs, y<-ys]

prod2 f xs ys = do
    x <- xs
    y <- ys
    return (f x y)

myGetLine :: IO String
myGetLine = getChar >>= \x ->
    if x == '\n' then
    return []
    else
    myGetLine >>= \xs -> return (x:xs)


myGetLine2 :: IO String
myGetLine2 = do
    x <- getChar
    if x == '\n' then
        return []
    else
        myGetLine2 >>= \xs -> return (x:xs)


prelNo noin = sqrt noin
ioNumber = do
    noin <- readLn :: IO Float
    putStrLn $ "Intrare\n" ++ (show noin)
    let noout = prelNo noin
    putStrLn $ "Iesire"
    print noout


prelNo2 noin = sqrt noin

ioNumber2 = 
    (readLn::IO Float)>>= \noin -> 
        putStrLn("Intrare\n"++show noin)>>= \_-> 
            let noout = prelNo noin in
                putStrLn "Iesire" >>= \_ -> print $ prelNo noin


----------------------------------------------------------------------------------------------

--- Monada Writer

newtype WriterS a = Writer { runWriter :: (a, String) } 


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


logIncrement :: Int -> WriterS Int
logIncrement x = do
    tell ("Increment " ++ show x ++ "\n")
    return (x+1)


logIncrement2 :: Int -> WriterS Int
logIncrement2 x = do
    y <- logIncrement x
    logIncrement y


logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x 0 = return x
logIncrementN x n = do
    y <- logIncrement x
    logIncrementN y (n-1)


--------------------------------------------------------------------------------------------


data Person = Person { name :: String, age :: Int }

showPersonN :: Person -> String
showPersonN p = "Name: " ++ name p ++ "\n"

showPersonA :: Person -> String
showPersonA p = "Age: " ++ show (age p) ++ "\n"

shwPerson :: Person -> String
shwPerson p = showPersonN p ++ showPersonA p

--------------------------------------------------------------------------------------------------------------

newtype Reader env a = Reader { runReader :: env -> a }


instance Monad (Reader env) where
  return x = Reader (\_ -> x)
  ma >>= k = Reader f
    where f env = let a = runReader ma env
                  in  runReader (k a) env



instance Applicative (Reader env) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance Functor (Reader env) where              
  fmap f ma = pure f <*> ma    



mshowPersonN :: Reader Person String
mshowPersonN = Reader (\p -> "Name: " ++ name p)

mshowPersonA :: Reader Person String
mshowPersonA = Reader (\p -> "Age: " ++ show (age p))

mshowPerson :: Reader Person String
mshowPerson = do
    n <- mshowPersonN
    a <- mshowPersonA
    return (n ++ ", " ++ a)



        

        


