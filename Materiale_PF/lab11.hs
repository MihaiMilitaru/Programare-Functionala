data List a = Nil
            | Cons a (List a)
        deriving (Eq, Show)


instance Functor List where
    fmap f Nil =Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where

    pure x = Cons x Nil

    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons f fs) <*> xs = (fmap f xs) ++ (fs <*> xs)
                        where
                            Nil ++ ys = ys
                            (Cons x xs) ++ ys = Cons x (xs ++ ys)

f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)
test1 = (f <*> v) == Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

data Cow = Cow {
    name :: String
    , age :: Int
    , weight :: Int
    } deriving (Eq, Show)   

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s = Just s

noNegative :: Int -> Maybe Int
noNegative a
    | a < 0 = Nothing
    | otherwise = Just a


cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString s a w = Cow
    <$> noEmpty s
    <*> noNegative a
    <*> noNegative w

test24 = cowFromString "Milka" 5 100 == Just (Cow {name = "Milka", age = 5, weight = 100})


newtype Name = Name String 
    deriving (Eq, Show)
newtype Address = Address String 
    deriving (Eq, Show)
data Person = Person Name Address
    deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String
validateLength maxLen s = 
    if length s > maxLen
        then Nothing
    else Just s


mkName :: String -> Maybe Name
mkName s = Name
    <$> validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress s = Address
    <$> validateLength 100 s


mkPerson :: String -> String -> Maybe Person
mkPerson n a = Person
    <$> mkName n
    <*> mkAddress a

