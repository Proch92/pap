data Elem = Dot | Star

---------------- Dichiarazione variabile "vec" ---------------
vec :: [Elem]
vec = [Dot, Star, Star, Star, Dot, Dot, Dot, Dot, Star, Star, Dot, Dot, Star, Star, Star, Star, Dot]
vec2 = [Dot, Dot, Star, Star, Star, Dot, Dot, Dot, Dot, Star, Star, Dot, Dot, Star, Star, Star, Star]

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

----------- maxStarSeq -----------------
countStarGroup :: [Elem] -> Int
countStarGroup (Dot:xs) = 0
countStarGroup [] = 0
countStarGroup (Star:xs) = 1 + countStarGroup xs

maxStarSeq :: [Elem] -> Int
maxStarSeq [] = 0
maxStarSeq (Star:xs) = maxStarSeq xs
maxStarSeq (Dot:xs) 
	| nextGroup > maxS 	= nextGroup
	| otherwise 		= maxS
	where
		nextGroup = countStarGroup xs
		maxS = maxStarSeq xs

---------- matchSeq --------------------
matchSeq :: [Elem] -> [Elem] -> Bool
matchSeq [] [] = True
matchSeq (Dot:xs) y = matchSeq xs y
matchSeq x (Dot:ys) = matchSeq x ys
matchSeq (Star:xs) (Star:ys) = matchSeq xs ys
matchSeq (Star:(Dot:xs)) (Star:(Star:ys)) = False
matchSeq (Star:(Star:xs)) (Star:(Dot:ys)) = False
matchSeq [] (Star:ys) = False
matchSeq (Star:xs) [] = False

--------- occ --------------------------
occ :: [Elem] -> [(Int, [Int])]