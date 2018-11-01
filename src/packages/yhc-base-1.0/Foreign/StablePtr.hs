module Foreign.StablePtr(StablePtr, newStablePtr, deRefStablePtr, freeStablePtr, castStablePtrToPtr, castPtrToStablePtr) where

import YHC.Primitive

newStablePtr :: a -> IO (StablePtr a)
newStablePtr a = primCreateStablePtr a

deRefStablePtr :: StablePtr a -> IO a
deRefStablePtr p = primDerefStablePtr p

freeStablePtr :: StablePtr a -> IO ()
freeStablePtr p = primFreeStablePtr p

castStablePtrToPtr :: StablePtr a -> Ptr ()
castStablePtrToPtr (StablePtr p) = p

castPtrToStablePtr :: Ptr () -> StablePtr a
castPtrToStablePtr p = StablePtr p


