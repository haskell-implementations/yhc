module Foreign.Ptr
  ( Ptr		-- abstract, instance of: Eq, Ord, Enum, Show
  , nullPtr	-- :: Ptr a
  , castPtr	-- :: Ptr a -> Ptr b
  , plusPtr	-- :: Ptr a -> Int -> Ptr b
  , alignPtr    -- :: Ptr a -> Int -> Ptr a
  , minusPtr    -- :: Ptr a -> Ptr b -> Int

  , FunPtr
  , nullFunPtr
  , castFunPtr
  , castFunPtrToPtr
  , castPtrToFunPtr
  , freeHaskellFunPtr
  ) where

import Numeric (showHex)
import YHC.Internal
import YHC.Primitive

--foreign import cast ptrToInt :: Ptr a -> Int
--foreign import cast intToPtr :: Int -> Ptr a
--foreign import cast castPtr  :: Ptr a -> Ptr b

instance Eq (Ptr a) where
  x == y        =  ptrToInt x == ptrToInt y
instance Ord (Ptr a) where
  compare x y   =  compare (ptrToInt x) (ptrToInt y)
instance Show (Ptr a) where
  showsPrec p a = showString "0x" . showHex (ptrToInt a)
instance Enum (Ptr a) where
  toEnum        = intToPtr
  fromEnum      = ptrToInt


nullPtr :: Ptr a
nullPtr  = intToPtr 0

plusPtr :: Ptr a -> Int -> Ptr b
plusPtr a i = intToPtr (ptrToInt a + i)

alignPtr :: Ptr a -> Int -> Ptr a
alignPtr a i = intToPtr (let j = ptrToInt a in j + (j`rem`i))

minusPtr :: Ptr a -> Ptr b -> Int
minusPtr a b = ptrToInt a - ptrToInt b

--foreign import cast funPtrToInt :: FunPtr a -> Int

instance Eq (FunPtr a) where
  a == b        = (funPtrToInt a) == (funPtrToInt b)
instance Ord (FunPtr a) where
  compare a b   = compare (funPtrToInt a) (funPtrToInt b)
instance Show (FunPtr a) where
  showsPrec _ p = showString "0x" . showHex (funPtrToInt p)

nullFunPtr :: FunPtr a
nullFunPtr = castPtrToFunPtr nullPtr

castFunPtr :: FunPtr a -> FunPtr b
castFunPtr p = unsafeCoerce p

freeHaskellFunPtr :: FunPtr a -> IO ()
freeHaskellFunPtr p = return ()         -- not implemented

--foreign import cast castFunPtrToPtr :: FunPtr a -> Ptr b
--foreign import cast castPtrToFunPtr :: Ptr a -> FunPtr b

