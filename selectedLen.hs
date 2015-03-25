type Word = String

selectedLen :: [Word] -> Int -> [(Word,Int)]
selectedLen list l = map (\x -> (x, length x)) (filter (\x -> length x > l) list)