module Foreign.Marshal.Alloc where

import Foreign.Ptr
import Foreign.Storable
import YHC.Primitive


alloca :: Storable a => (Ptr a -> IO b) -> IO b
alloca f =
  do p      <- malloc
     result <- f p
     free p
     return result

allocaBytes :: Int -> (Ptr a -> IO b) -> IO b
allocaBytes n f =
  do p      <- mallocBytes n
     result <- f p
     free p
     return result

malloc :: Storable a => IO (Ptr a)
malloc = mallocBytes (sizeOf a)
  where
  a = undefined :: a

mallocBytes :: Int -> IO (Ptr a)
mallocBytes n = primMalloc n

realloc :: Storable b => Ptr a -> IO (Ptr b)
realloc p = primRealloc p (sizeOf b)
  where
  b = undefined :: b

reallocBytes :: Ptr a -> Int -> IO (Ptr a)
reallocBytes p n = primRealloc p n

free :: Ptr a -> IO ()
free p = primFree p

finalizerFree :: FinalizerPtr a
finalizerFree = primFinalizerFree ()

