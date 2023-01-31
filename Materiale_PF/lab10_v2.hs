newtype Identity a = Identity a
        deriving Show

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)


data Pair a = Pair a a
        deriving Show

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)


data Constant a b = Constant b
       deriving Show

instance Functor (Constant a) where
    fmap f (Constant b) = Constant (f b)


data Two a b = Two a b
       deriving Show

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)


data Three a b c = Three a b c
        deriving Show

instance Functor (Three a b) where
    fmap :: (a2 -> b2) -> Three a1 b1 a2 -> Three a1 b1 b2
    fmap f (Three a b c) = Three a b (f c)

data Three' a b = Three' a b b
         deriving Show

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)


data Four a b c d = Four a b c d
        deriving Show

instance Functor (Four a b c ) where
    fmap f (Four a b c d) = Four a b c (f d)

    
data Four'' a b = Four'' a a a b
        deriving Show

instance Functor (Four'' a) where
    fmap f (Four'' a b c d) = Four'' a b c (f d)


data Quant a b = Finance | Desk a | Bloor b
        deriving Show

instance Functor (Quant a) where
    fmap f (Finance) = Finance
    fmap f (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)

----------------------------------------------------------


data LiftItOut f a = LiftItOut (f a)
        deriving Show
instance Functor f => Functor (LiftItOut f) where
    fmap g (LiftItOut fa) = LiftItOut (fmap g fa)


data Parappa f g a = DaWrappa (f a) (g a)
        deriving Show
instance (Functor f , Functor g) => Functor (Parappa f g) where
    fmap ff (DaWrappa fa ga) = DaWrappa (fmap ff fa) (fmap ff ga)

    
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
        deriving Show
instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
    fmap fu (IgnoringSomething fa ga) = IgnoringSomething fa (fmap fu ga)
    

data Notorious g o a t = Notorious (g o) (g a) (g t)
        deriving Show
instance Functor g => Functor (Notorious g o a) where
    fmap ff (Notorious go ga gt) = Notorious go ga (fmap ff gt)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
        deriving Show
instance Functor GoatLord where
    fmap ff (NoGoat) = NoGoat 
    fmap ff (OneGoat a) = OneGoat (ff a)
    fmap ff (MoreGoats ga gb gc) = MoreGoats (fmap ff ga) (fmap ff gb) (fmap ff gc)

data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where
    fmap ff (Halt) = Halt
    fmap ff (Print s a) = Print s (ff a)
    fmap ff (Read sa) = Read (fmap ff sa)

