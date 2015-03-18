data BSTree a = Nil | Node a (BSTree a) (BSTree a)


testTree :: BSTree String
testTree = (Node "faro"
				(Node "cacao"
					(Node "albero" Nil Nil)
					(Node "dado" Nil  Nil))
				(Node "luce"
					(Node "iodio" Nil Nil)
					Nil))

ispresentinbst :: (Ord a) => BSTree a -> a -> Bool
ispresentinbst Nil _ = False
ispresentinbst (Node nodo sx dx) y 
	| nodo == y 	= True
	| y < nodo		= ispresentinbst sx y
	| otherwise		= ispresentinbst dx y