{- Tests for a bug in the bytecode backend that couldn't handle
   CoreApp (CoreCase ...) ... -}

getSP f = f ()
putSP x y = x

filterSP p = getSP (\x -> (if p x then putSP x else id) (filterSP p))


main = print $ filterSP (const True)
