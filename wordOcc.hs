type Word = String

wordOcc :: [Word] -> Word -> Int
wordOcc list word = foldr (\parola reduction -> if (word == parola) then reduction + 1 else reduction) 0 list