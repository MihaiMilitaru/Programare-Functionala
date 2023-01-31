data Punct = Pt [Int]

data Arb = Vid | F Int | N Arb Arb
    deriving Show

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a


--1a

instance Show Punct  where
    show::Punct -> String
    show(Pt[]) = "()"
    show(Pt(x:xs)) = "(" ++ showSec x xs ++ ")"
        where 
            showSec x [] = show x
            showSec x (y:ys)= show x ++ ", " ++ showSec y ys


--1b

--Exercitiul 2 
--a

data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
    perimeter :: (Floating a) => g a -> a
    area :: (Floating a) => g a -> a    



instance GeoOps Geo where
    perimeter :: (Floating a) => Geo a-> a
    perimeter (Square a)= 4 * a
    perimeter (Rectangle  a b )= 2 * a + 2*b
    perimeter (Circle a)= 2 * pi * a



    area :: (Floating a) => Geo a-> a
    
    area (Square a)=a*a

   
    area (Rectangle a b)=a*b
    area (Circle a)= pi * a*a

   
--b

instance (Floating a, Eq a) => Eq(Geo a) where
    (==) f1 f2 = perimeter f1 == perimeter f2
    
