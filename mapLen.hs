type Word = String

mapLen :: [Word] -> [(Word,Int)]
mapLen l = map (\x -> (x, length x)) l