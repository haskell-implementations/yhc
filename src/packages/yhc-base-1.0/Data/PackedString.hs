{-# OPTIONS_YHC --unifyhack #-}
module Data.PackedString
    (
     PackedString,
     unpackPS,
     packString
    )
    where

import PreludeBuiltin
import Foreign.Ptr
import Foreign.Util
import YHC.Primitive

unpackPS :: PackedString -> [Char]
unpackPS ps = unpackPSAt ps 0
  where
  unpackPSAt ps n = let c = primPSGetChar ps n
                    in if c == '\0' then []
                                    else c : unpackPSAt ps (n+1)

packString :: String -> PackedString
packString str = primPackString (length str) (seqList str)

comparePS :: PackedString -> PackedString -> Ordering
comparePS x y
  | n < 0  = LT
  | n == 0 = EQ
  | n > 0  = GT
  where
  n = primComparePS x y

instance Eq PackedString where
    p1 == p2 = comparePS p1 p2 == EQ
    p1 /= p2 = comparePS p1 p2 /= EQ

instance Ord PackedString where
    compare p1 p2 = comparePS p1 p2

