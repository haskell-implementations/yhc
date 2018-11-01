module Foreign.Util where

seqList :: [a] -> [a]
seqList xs = seq (force xs) xs
  where
  force []     = ()
  force (x:xs) = seq x (force xs)

