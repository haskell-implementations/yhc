-- | FIXME: many things still need doing here ..
module Foreign.C.String(CString, peekCString, newCString, freeCString, withCString,
                        peekCAString, newCAString, withCAString) where

import YHC.Primitive
import PreludeBuiltin(_unpackString)
import Foreign.Util
import Foreign.Storable

-- the strictness of peekCString is tricky, particularly since it is common to do
--    xs <- peekCString s
--    freeString s
-- thus it really needs to be 100% strict
{- Tom: BROKEN, trying a different method
peekCString :: CString -> IO String
peekCString cs = xs `seq` return xs
  where
  xs        = seqList (unpack cs)
  unpack cs = _unpackString cs
-}

peekCString :: CString -> IO String
peekCString (CString ptr) = unpack 0
    where
    unpack i = do
        c <- peekElemOff ptr i
        if c == '\0' then
            return []
         else do
            xs <- unpack (i+1)
            return (c:xs)

newCString :: String -> IO CString
newCString s = primNewCString (length s) (seqList s)

freeCString :: CString -> IO ()
freeCString cs = primFreeCString cs

withCString :: String -> (CString -> IO a) -> IO a
withCString s f =
  do cs <- newCString s
     r <- f cs
     freeCString cs
     return r

peekCAString = peekCString
newCAString = newCString
withCAString = withCString
