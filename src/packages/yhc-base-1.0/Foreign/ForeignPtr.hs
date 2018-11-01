-- | ForeignPtr is a references to foreign value that is associated with some finalizer. When
-- | Haskell detects that the reference is no longer used within the Haskell program the
-- | finalizer (written in the foreign language) is executed.

module Foreign.ForeignPtr (
     ForeignPtr, FinalizerPtr,
     newForeignPtr, newForeignPtr_,
     addForeignPtrFinalizer,
     withForeignPtr,
     finalizeForeignPtr,
     unsafeForeignPtrToPtr,
     touchForeignPtr,
     castForeignPtr,
--     mallocForeignPtr,
--     mallocForeignPtrBytes,
--     mallocForeignPtrArray,
--     mallocForeignPtrArray0
     )where

import YHC.Internal
import YHC.Primitive
import Foreign.Ptr

instance Eq (ForeignPtr a) where
    x == y = (unsafeForeignPtrToPtr x) == (unsafeForeignPtrToPtr y)

instance Ord (ForeignPtr a) where
    compare x y = compare (unsafeForeignPtrToPtr x) (unsafeForeignPtrToPtr y)
    

newForeignPtr_ :: Ptr a -> IO (ForeignPtr a)
newForeignPtr_ a = primCreateForeignPtr a

addForeignPtrFinalizer :: FinalizerPtr a -> ForeignPtr a -> IO ()
addForeignPtrFinalizer z fp = primAttachForeignPtr z fp

newForeignPtr :: FinalizerPtr a -> Ptr a -> IO (ForeignPtr a)
newForeignPtr z p = do fp <- newForeignPtr_ p
                       addForeignPtrFinalizer z fp
                       return fp

withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
withForeignPtr fp f = let p = unsafeForeignPtrToPtr fp
                      in do result <- f p
                            touchForeignPtr fp
                            return result

finalizeForeignPtr :: ForeignPtr a -> IO ()
finalizeForeignPtr fp = primFreeForeignPtr fp

unsafeForeignPtrToPtr :: ForeignPtr a -> Ptr a
unsafeForeignPtrToPtr fp = primDerefForeignPtr fp

touchForeignPtr :: ForeignPtr a -> IO ()
touchForeignPtr fp = return ()

castForeignPtr :: ForeignPtr a -> ForeignPtr b
castForeignPtr fp = unsafeCoerce fp









