-- nummer 1

fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib(n-2)

-- nummer 2

fibs 0 = []
fibs n = fib(n) : fibs(n-1)

-- nummer 4

nTes (x:xs) num = if (num == 0) then x else nTes xs (num-1)

-- nummer 5

-- a

penultimate (x:_:[]) = x
penultimate (_:xs) = penultimate xs

-- b

penultimateB a = a !! ((length a) -2) 

-- c

penultimateC a = last (init a)

-- nummer 6

concat_ [] = []
concat_ (x:xs) = x ++ concat_ xs

-- nummer 8

countOdd [] = 0
countOdd (x:xs) = if (mod x 2 == 1) then (1 + (countOdd xs)) else countOdd xs