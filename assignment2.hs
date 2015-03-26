import GHC.Float
import Screen

type P2d = (Int, Int)
type V2d = (Int, Int)

data Shape = Line P2d P2d | Triangle P2d P2d P2d | Rectangle P2d Int Int | Circonference P2d Int | Composition [Shape]
data BSTree a = Nil | Node a (BSTree a) (BSTree a)

-------------------------- utilities ------------------------------

distance :: P2d -> P2d -> Float
distance (ax, ay) (bx, by) = sqrt (int2Float((bx - ax)^2) + int2Float((by - ay)^2))

add :: P2d -> V2d -> P2d
add (px, py) (vx, vy) = ((px+vx), (py+vy))

outer :: P2d -> P2d -> Bool
outer (pointx, pointy) (boundx, boundy) = (pointx >= boundx) && (pointy >= boundy)
inner :: P2d -> P2d -> Bool
inner (pointx, pointy) (boundx, boundy) = (pointx <= boundx) && (pointy <= boundy)

lesserx :: Shape -> Shape -> Bool
lesserx s1 s2
	| getXpos s1 < getXpos s2 	= True
	| otherwise 				= False

pair :: [a] -> [b] -> [(a,b)]
pair [] [] = []
pair [] _ = error "different lengths... cannot pair"
pair _ [] = error "different lengths... cannot pair"
pair (x:xs) (y:ys) = (x,y):(pair xs ys)

deg2rad :: Int -> Float
deg2rad deg = (int2Float deg) * (3.14159265359 / 180.0)

-------------------------- classe CShape ------------------------------

class CShape a where
	perim :: a -> Float
	move :: a -> V2d -> Shape
	area :: a -> Float
	getXpos :: a -> Int

instance CShape Shape where
	perim (Line a b) = error "Not defined"
	perim (Triangle a b c) = distance a b + distance a c + distance b c
	perim (Rectangle _ w h) = int2Float (2 * (w + h))
	perim (Circonference _ radius) = 2 * 3.14 * int2Float radius
	perim (Composition l) = foldr (\x sum -> perim x + sum) 0.0 l

	move (Line a b) v = (Line (add a v) (add b v))
	move (Triangle a b c) v = (Triangle (add a v) (add b v) (add c v))
	move (Rectangle pos w h) v = (Rectangle (add pos v) w h)
	move (Circonference center radius) v = (Circonference (add center v) radius)
	move (Composition list) v = (Composition (map (\x -> move x v) list))

	area (Line _ _) = 0.0
	area (Triangle a b c) = sqrt ( -- formula di erone http://it.wikipedia.org/wiki/Formula_di_Erone
		per * (per - distance a b) * (per - distance b c) * (per - distance a c))
		where per = perim (Triangle a b c) / 2
	area (Rectangle _ w h) = int2Float (w * h)
	area (Circonference _ radius) = 3.14 * (int2Float (radius^2))
	area (Composition list) = foldr (\x a -> area x + a) 0.0 list

	getXpos (Line (ax, _) (bx, _)) = min ax bx
	getXpos (Triangle (ax, _) (bx, _) (cx, _)) = min ax (min bx cx)
	getXpos (Rectangle (posx, _) _ _) = posx
	getXpos (Circonference (posx, _) radius) = posx - radius
	getXpos (Composition list) = foldr (\x m -> min (getXpos x) m) (maxBound :: Int) list
	
-------------------------- funzioni aggiuntive ------------------------------
moveShapes :: [Shape] -> V2d -> [Shape]
moveShapes l v = map (\x -> move x v) l

isInBBox :: Shape -> P2d -> P2d -> Bool
isInBBox (Line a b) p1 p2 = and [outer a p1, outer b p1, inner a p2, inner b p2]
isInBBox (Triangle a b c) p1 p2 = and [outer a p1, outer b p1, outer c p1, inner a p2, inner b p2, inner c p2]
isInBBox (Rectangle pos w h) p1 p2 = and [outer pos p1, inner (add pos (w, h)) p2]
isInBBox (Circonference center radius) p1 p2 = and [
	outer (add center (-radius, 0)) p1,
	outer (add center (0, -radius)) p1,
	inner (add center (radius, 0)) p2,
	inner (add center (0, radius)) p2]
isInBBox (Composition list) p1 p2 = foldr (\x b -> and [isInBBox x p1 p2, b]) True list


inBBox :: [Shape] -> P2d -> P2d -> [Shape]
inBBox l p1 p2 = filter (\x -> isInBBox x p1 p2) l

maxArea :: [Shape] -> Shape
maxArea (x:xs)
	| area x > area (maxArea xs) = x
	| otherwise = maxArea xs

insertInTree :: BSTree Shape -> Shape -> BSTree Shape
insertInTree Nil shape = (Node shape Nil Nil)
insertInTree (Node s sx dx) shape
	| lesserx shape s 	= (Node s (insertInTree sx shape) dx)
	| otherwise			= (Node s sx (insertInTree sx shape))

makeShapeTree :: [Shape] -> BSTree Shape
makeShapeTree [] = Nil
makeShapeTree (x:xs) = insertInTree (makeShapeTree xs) x

------------------- draw stuff ----------------------------
linearInterpolation :: Int -> Int -> Int -> [Int] -- start, end, step
linearInterpolation _ end 100 = end:[]
linearInterpolation start end step = 
	float2Int ((int2Float start) + ((int2Float step / 100) * int2Float (end - start)))
	: linearInterpolation start end (step + 1)

sinInterpolation :: Int -> Int -> [Int]
sinInterpolation 360 _ = 0:[]
sinInterpolation deg radius = float2Int (sin (deg2rad deg) * (int2Float radius)) : sinInterpolation (deg + 1) radius

cosInterpolation :: Int -> Int -> [Int]
cosInterpolation 360 radius = (1 * radius):[]
cosInterpolation deg radius = float2Int (cos (deg2rad deg) * (int2Float radius)) : cosInterpolation (deg + 1) radius

interpolateLine :: Shape -> [IO()]
interpolateLine (Line (ax, ay) (bx, by)) = map (\p -> writeAt p "*") (pair (linearInterpolation ax bx 0) (linearInterpolation ay by 0))

interpolateRad :: Shape -> [IO()]
interpolateRad (Circonference (centerx, centery) radius) = map (\p -> writeAt p "*") (map (\(x,y) -> (x + centerx, y + centery)) (pair (sinInterpolation 0 radius) (cosInterpolation 0 radius)))

drawAll :: [Shape] -> IO()

class (CShape a) => Drawable a where
	draw :: a -> IO()

instance Drawable Shape where
	draw (Line a b) = foldr (>>) (goto (0,50)) (interpolateLine (Line a b))
	draw (Triangle a b c) = do
		draw (Line a b)
		draw (Line b c)
		draw (Line a c)
	draw (Rectangle (posx, posy) w h) = do
		draw (Line tl tr)
		draw (Line bl br)
		draw (Line tl bl)
		draw (Line tr br)
		where
			tl = (posx, posy)
			tr = (posx+w, posy)
			bl = (posx, posy+h)
			br = (posx+w, posy+h)
	draw (Circonference center radius) = foldr (>>) (goto (0,50)) (interpolateRad (Circonference center radius))
	draw (Composition list) = drawAll list

drawAll list = foldr (>>) (goto (0,50)) (map (\x -> draw x) list)