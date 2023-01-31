functie1 :: Int -> Int -> Int
functie1 x y = x*x + y*y

functie2 :: Int -> String
functie2 x = if even x then "par" else "impar"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

functie3 :: Int -> Int -> Bool
functie3 x y = if x > 2 * y then True else False

