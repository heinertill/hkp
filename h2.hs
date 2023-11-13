-- nummer 1

productList x = foldr (*) 1 x

-- nummer 2

boolFold x = foldr (&&) True x

-- nummer 3

-- filter' b (x:[]) = [b x]
-- filter' b (x:xs) = b x : filter' b xs
filter' p xs = foldr (\elem res -> if p elem then (elem:res) else res) [] xs

-- nummer 4

dec2nat x = foldl (\acc x -> acc*10 + x) 0 x

-- nummer 5

cubicZ = [x*x*x | x <- [1..]]

prim p = and [mod p n > 0 | n <- 2: [3,5..p], n*n <= p]
primZ = [p | p <- [2..], prim p]

evenodd = [(x,y) | x <- [0,2..], y <- [1,3..]]

-- nummer 6

split [] = ([], [])
split [x] = ([x], [])
split (x:y:xs) = let (left, right) = split xs in (x:left, y:right) 

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y then x : merge xs (y:ys) else y : merge (x:xs) ys

mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = let (left, right) = split xs in merge (mergeSort left) (mergeSort right)
