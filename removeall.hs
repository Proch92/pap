removeall :: (Eq a) => [a] -> a -> [a]
removeall [] _ = []
removeall (x:xs) y
	| x == y		= res
	| otherwise		= x : res
	where res = removeall xs y