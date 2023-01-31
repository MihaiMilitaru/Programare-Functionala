import Data.List

myInt = 5555555555555555555555555555555555555555555555555555555555555555555555555555555555555

double :: Integer -> Integer
double x = x+x

triple x = x*3


--maxim :: Integer -> Integer -> Integer
maxim x y = if (x > y)
               then x
          else y

max3 x y z = let
             u = maxim x y
             in (maxim  u z)

               
-- Exercitiile ca tema sunt mai jos

sum_squares :: Integer -> Integer -> Integer 

sum_squares x y = x ^ 2 + y ^ 2


odd_even :: Integer -> String

odd_even x = if (x `mod` 2 == 0)
                    then "Par"
          else "Impar"


factorial :: Integer -> Integer

factorial x = if (x==1)
                    then 1
          else (factorial (x-1))*x


greater x y = if (x > 2*y)
                    then True
          else False


x = let x = 3; y = 4 in x * y