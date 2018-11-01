module Foreign.Marshal.Utils where

import YHC.Primitive
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

with :: Storable a => a -> (Ptr a -> IO b) -> IO b
with v f = alloca (\p -> poke p v >> f p)

new :: Storable a => a -> IO (Ptr a)
new v = do
  p <- malloc
  poke p v
  return p

fromBool :: Num a => Bool -> a
fromBool True = 1
fromBool False = 0

toBool :: Num a => a -> Bool
toBool 0 = False
toBool _ = True

maybeNew :: (a -> IO (Ptr a)) -> Maybe a -> IO (Ptr a)
maybeNew f Nothing = return nullPtr
maybeNew f (Just a) = f a

maybeWith :: (a -> (Ptr b -> IO c) -> IO c) -> Maybe a -> (Ptr b -> IO c) -> IO c
maybeWith f Nothing g = g nullPtr
maybeWith f (Just a) g = f a g

maybePeek :: (Ptr a -> IO b) -> Ptr a -> IO (Maybe b)
maybePeek f p
  | p == nullPtr = return Nothing
  | otherwise    = do v <- f p
                      return (Just v)

-- FIXME: what does this do??
-- withMany :: (a -> (b -> res) -> res) -> [a] -> ([b] -> res) -> res
-- withMany f as g =

copyBytes :: Ptr a -> Ptr a -> Int -> IO ()
copyBytes dst src num = primMemcpy dst src num

moveBytes :: Ptr a -> Ptr a -> Int -> IO ()
moveBytes dst src num = primMemmove dst src num
