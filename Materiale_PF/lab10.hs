newtype Identity a = Identity a

instance Functor Identity where
    fmap :: (a->b) -> Identity a -> Identity b
    fmap aToB (Identity a) = 
        Identity (aToB a)


data Pair a = Pair a a

instance Functor Pair where
    fmap :: (a -> b) -> Pair a -> Pair b
    fmap f (Pair a b) = Pair (f a) (f b)



data Constant a b = Constant b
instance Functor (Constant  a) where
    fmap f (Constant b) = Constant (f b)


data Two a b = Two a b 
instance Functor (Two a ) where
    fmap f (Two a b) = Two a (f b)

data Three a b c = Three a b c
instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)


data Three' a b = Three' a b b
instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

data Four a b c d = Four a b c d
instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

data Four'' a b = Four'' a a a b
instance Functor (Four'' a) where
    fmap f (Four'' a b c d) = Four'' a b c (f d)


data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a) where
    fmap f (Finance ) = Finance 
    fmap f (Desk a) = Desk a
    fmap f (Bloor a) = Bloor (f a)


data LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
    fmap g (LiftItOut fa) = LiftItOut (fmap g fa)


data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f , Functor g)=> Functor (Parappa f g) where
    fmap ff (DaWrappa aa bb) = DaWrappa (fmap ff aa) (fmap ff bb)


data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
    fmap ff (IgnoringSomething aa bb) =  IgnoringSomething aa (fmap ff bb)

