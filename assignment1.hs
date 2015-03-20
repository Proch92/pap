data Elem = Dot | Star

---------------- Dichiarazione variabile "vec" ---------------
vec :: [Elem]
vec = [Star, Star, Star, Star, Dot, Dot, Dot, Dot, Star, Star, Dot, Dot, Star, Star, Star, Dot]
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
nextCl :: [Elem] -> Int
nextCl (Dot:xs) = 0
nextCl [] = 0
nextCl (Star:xs) = 1 + nextCl xs

maxStarSeq :: [Elem] -> Int
maxStarSeq [] = 0
maxStarSeq (x:xs) 
	| nextCluster > maxS 	= nextCluster
	| otherwise 			= maxS
	where
		nextCluster = nextCl (x:xs)
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

{-
--------- occ --------------------------
occ :: [Elem] -> [(Int, [Int])]
occ [] = []
occ (Star:xs) = (, ) : occ xs
occ (Dot:xs) = 
-}

-------------- bst ---------------
data BSTree a = Nil | Node a (BSTree a) (BSTree a)

countStarInTree :: BSTree Elem -> Int
countStarInTree Nil = 0
countStarInTree (Node Star sx dx) = 1 + countStarInTree sx + countStarInTree dx
countStarInTree (Node Dot sx dx) = countStarInTree sx + countStarInTree dx

testTree :: BSTree Elem
testTree = (Node Dot
				(Node Star
					(Node Star 
						(Node Dot Nil Nil)
					Nil)
					(Node Star Nil Nil))
				(Node Star
					(Node Star 
						(Node Star Nil Nil)
					Nil)
					Nil))

--------------------- pathTree --------------
pathTree :: BSTree Elem -> Int
pathTree Nil = 0
pathTree (Node Dot sx dx) = 0
pathTree (Node Star sx dx)
	| pathTree sx > pathTree dx 	= 1 + pathTree sx
	| pathTree sx <= pathTree dx 	= 1 + pathTree dx
