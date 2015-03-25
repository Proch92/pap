type P2d = (Int, Int)
type V2d = (Int, Int)

data Shape = Line P2d P2d | Triangle P2d P2d P2d | Rectangle P2d Int Int | Circonference P2d V2d | Composition [Shape]

-------------------------- utilities ------------------------------

distance :: P2d -> P2d -> Float
distance (ax, ay) (bx, by) = sqrt (((bx - ax)(^2)) + ((by - ay)(^2)))

add :: P2d -> V2d -> P2d
add (px, py) (vx, vy) = ((px+vx), (py+vy))

outer :: P2d -> P2d -> Bool
outer (pointx, pointy) (boundx, boundy) = (pointx >= boundx) && (pointy >= boundy)
inner :: P2d -> P2d -> Bool
inner (pointx, pointy) (boundx, boundy) = (pointx <= boundx) && (pointy <= boundy)

-------------------------- classe CShape ------------------------------

class CShape a where
	perim :: a -> Float
	move :: a -> V2d -> Shape

instance CShape Shape where
	perim (Line a b) = error "Not defined"
	perim (Triangle a b c) = distance a b + distance a c + distance b c
	perim (Rectangle _ w h) = 2 * (w + h)
	perim (Circonference _ radius) = 3.14 * (radius(^2))
	perim (Composition _) = error "Not defined"

	move :: Shape -> V2d -> Shape
	move (Line a b) v = (Line (add a v) (add b v))
	move (Triangle a b c) v = (Triangle (add a v) (add b v) (add c v))
	move (Rectangle pos w h) v = (Rectangle (add pos v) w h)
	move (Circonference center radius) v = (Circonference (add center v) radius)
	move (Composition list) = map (\x -> move x) list

-------------------------- funzioni aggiuntive ------------------------------
moveShapes :: [Shape] -> [Shape]
moveShapes l v = map (\x -> move x v) l

isInBBox :: Shape -> P2d -> P2d -> Bool
isInBBox (Line a b) p1 p2 = and [outer a p1, outer b p1, inner a p2, inner b p2]
isInBBox (Triangle a b c) p1 p2 = and [outer a p1, outer b p1, outer c p1, inner a p2, inner b p2, inner c p2]
isInBBox (Rectangle pos w h) p1 p2 = and [outer pos p1, inner (add pos (w, h)) p2]
--isInBBox (Circonference center radius) p1 p2 = and []


inBBox :: [Shape] -> P2d -> P2d -> [Shape]
inBBox l p1 p2 = filter (\x -> isInBBox x p1 p2) l