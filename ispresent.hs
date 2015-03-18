ispresent :: (Eq a) => [a] -> a -> Bool
ispresent [] _ = False
ispresent (x:xs) y
	| x == y 		= True
	| otherwise 	= ispresent xs y