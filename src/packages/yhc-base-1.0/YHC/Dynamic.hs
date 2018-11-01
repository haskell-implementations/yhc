module YHC.Dynamic (PolyTypeRep, TypeRep, typeRep, toTypeRep) where

import YHC.Internal


instance Show TypeRep where
    show (TyCon s xs) = "(TyCon "++show s++" "++show xs++")"
    show (TyGen s) = "(TyGen "++show s++")"

instance Show a => Show (PolyTypeRep a) where
    show ptr = show (toTypeRep ptr)

interleave :: [a] -> [[a]] -> [a]
interleave x [] = []
interleave x ys = foldr1 (\y z -> y ++ x ++ z) ys

