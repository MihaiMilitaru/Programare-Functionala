import Data.Monoid

elem1 :: (Foldable t, Eq a) => a -> t a -> Bool
elem1 search = foldr cmp False
        where 
            cmp x xs
                | x == search = True
                | otherwise = xs 


null1 :: (Foldable t) => t a -> Bool
null1 = foldr f True
        where f _ _ =False


length1 :: (Foldable t) => t a -> Int
length1 = foldr f 0
        where f x sum = sum+1


toList1 :: (Foldable t) => t a -> [a]
toList1 = foldr (:) []


fold1 :: (Foldable t, Monoid m) => t m -> m
fold1 = foldMap f
        where f = id




data Constant a b = Constant b
instance Foldable (Constant a) where
    foldMap f (Constant b) = f b


data Two a b = Two a b
instance Foldable (Two a) where
    foldMap f (Two a b) = f b
    
data Three a b c = Three a b c
instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

data Three' a b = Three' a b b
instance Foldable (Three' a) where
    foldMap f (Three' a b c) = (f b) <> (f c)


data Four' a b = Four' a b b b
instance Foldable (Four' a) where
    foldMap f (Four' a b c d) = (f b) <> (f c) <> (f d)


data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Foldable GoatLord where
    foldMap f (NoGoat) = mempty
    foldMap f (OneGoat a) = f a
    foldMap f (MoreGoats a b c) = foldMap f a <> foldMap f b <> foldMap f c

