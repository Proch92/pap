data Elem = Dot | Star

---------------- Dichiarazione variabile "vec" ---------------
vec :: [Elem]
vec = [Dot, Star, Dot, Dot, Dot, Dot, Star, Star, Dot, Dot]

---------------- countStar -----------------------
countStar :: [Elem] -> Int
countStar [] = 0
countStar (Star:xs) = 1 + countStar xs
countStar (Dot:xs) = countStar xs

---------------- printableSeq --------------------
printableSeq :: [Elem] -> [Char]
printableSeq [] = ""
printableSeq (Star:xs) = '*' : printableSeq xs
printableSeq (Dot:xs) = '.' : printableSeq xs

--------------- swapSeq -------------------
swapSeq :: [Elem] -> [Elem]
swapSeq [] = []
swapSeq (Star:xs) = Dot : swapSeq xs
swapSeq (Dot:xs) = Star : swapSeq xs

------------- zipSeq ------------------
zipSeq :: [Elem] -> [Elem]
zipSeq [] = []
zipSeq (Star:xs) = Star : zipSeq xs
zipSeq (Dot:(Dot:xs)) = zipSeq (Dot:xs)
zipSeq (Dot:(Star:xs)) = Dot : (Star : zipSeq xs)
zipSeq (Dot:[]) = [Dot]
