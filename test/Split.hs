split :: Eq a => [a] -> a -> [[a]]
split cs sep = split' cs sep []
    where
    split' []     s acc = [reverse acc]
    split' (c:cs) s acc | c == s    = (reverse acc) : split' cs s []
                        | otherwise = split' cs s (c:acc)
