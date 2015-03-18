append :: [a] -> [a] -> [a]
append [] xs = xs
append (x:xs) ys = x : append xs ys