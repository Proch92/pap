data Elem = Dot | Star

countStar :: [Elem] -> Int
countStar [] = 0
countStar (Star:xs) = 1 + countStar xs
countStar (Dot:xs) = countStar xs

vec :: [Elem]
vec = [Dot, Star, Dot, Star, Star]