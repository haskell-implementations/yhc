{-# OPTIONS_YHC --cpp #-}
module Foreign.Storable
  ( Storable
      ( sizeOf    -- :: a -> Int
      , alignment -- :: a -> Int
      , peekElemOff -- :: Ptr a -> Int      -> IO a
      , pokeElemOff -- :: Ptr a -> Int -> a -> IO ()
      , peekByteOff -- :: Ptr b -> Int      -> IO a
      , pokeByteOff -- :: Ptr b -> Int -> a -> IO ()
      , peek    -- :: Ptr a             -> IO a
      , poke    -- :: Ptr a        -> a -> IO ()
      , destruct  -- :: Ptr a             -> IO ()
      )
  ) where

import YHC.Internal
import YHC.Primitive
import Foreign.Int
import Foreign.Word
import Foreign.Ptr
--import StablePtr  (StablePtr)
--import CTypes
--import CTypesISO

class Storable a where

   -- Yields the storage requirements (in bytes) of the argument.
   -- * Never uses its argument.
   sizeOf      :: a -> Int

   -- Yields the alignment constraint of the argument.
   -- * An alignment constraint x is fulfilled by any address divisible by x.
   -- * Never uses its argument.
   alignment   :: a -> Int

   -- Read/write elements from an array of elements of the given type.
   peekElemOff :: Ptr a -> Int      -> IO a
   pokeElemOff :: Ptr a -> Int -> a -> IO ()

   -- The same with *byte* offsets.
   peekByteOff :: Ptr b -> Int      -> IO a
   pokeByteOff :: Ptr b -> Int -> a -> IO ()

   -- ... and with no offsets at all.
   peek        :: Ptr a      -> IO a
   poke        :: Ptr a -> a -> IO ()

   -- Free memory associated with the object
   -- (except the object pointer itself).
   destruct    :: Ptr a -> IO ()

   -- circular default instances
   peekElemOff = peekElemOff_ undefined
      where peekElemOff_ :: Storable a => a -> Ptr a -> Int -> IO a
            peekElemOff_ undef ptr off = peekByteOff ptr (off * sizeOf undef)
   pokeElemOff ptr off val = pokeByteOff ptr (off * sizeOf val) val

   peekByteOff ptr off = peek (ptr `plusPtr` off)
   pokeByteOff ptr off = poke (ptr `plusPtr` off)

   peek ptr = peekElemOff ptr 0
   poke ptr = pokeElemOff ptr 0

-- Note that the various `peek' and `poke' functions might require properly
-- aligned addresses to function correctly. This is architecture dependent;
-- thus, portable code should ensure that when peeking or poking values of
-- some type `a', the alignment constraint for `a', as given by the function
-- alignment is fulfilled.

   destruct _ = return ()

----------------------------------------------------------------------
-- system-dependent instances

instance Storable Bool where
   sizeOf        = const 1
   alignment     = const 1
   peek p        = primReadCharAtAddr (castPtr p) >>= return . toEnum . fromEnum
   poke p        = primWriteCharAtAddr (castPtr p) . toEnum . fromEnum

instance Storable Char where
   sizeOf        = const 1
   alignment     = const 1
   peek          = primReadCharAtAddr
   poke          = primWriteCharAtAddr

instance Storable Int where
   sizeOf        = const 4
   alignment     = const 4
   peek          = primReadIntAtAddr
   poke          = primWriteIntAtAddr

instance Storable (Ptr a) where
   sizeOf        = const 4
   alignment     = const 4
   peek          = primReadAddrAtAddr
   poke          = primWriteAddrAtAddr

instance Storable Float where
   sizeOf        = const 4
   alignment     = const 4
   peek          = primReadFloatAtAddr
   poke          = primWriteFloatAtAddr

instance Storable Double where
   sizeOf        = const 8
   alignment     = const 8
   peek          = primReadDoubleAtAddr
   poke          = primWriteDoubleAtAddr

instance Storable Word8 where
   sizeOf        = const 1
   alignment     = sizeOf   -- not sure about this
   peek          = primReadWord8AtAddr
   poke          = primWriteWord8AtAddr

instance Storable Word16 where
   sizeOf        = const 2
   alignment     = sizeOf   -- not sure about this
   peek          = primReadWord16AtAddr
   poke          = primWriteWord16AtAddr

instance Storable Word32 where
   sizeOf        = const 4
   alignment     = sizeOf   -- not sure about this
   peek          = primReadWord32AtAddr
   poke          = primWriteWord32AtAddr

instance Storable Word64 where
   sizeOf        = const 8
   alignment     = sizeOf   -- not sure about this
   peek          = primReadWord64AtAddr
   poke          = primWriteWord64AtAddr

instance Storable Int8 where
   sizeOf        = const 1
   alignment     = sizeOf   -- not sure about this
   peek          = primReadInt8AtAddr
   poke          = primWriteInt8AtAddr

instance Storable Int16 where
   sizeOf        = const 2
   alignment     = sizeOf   -- not sure about this
   peek          = primReadInt16AtAddr
   poke          = primWriteInt16AtAddr

instance Storable Int32 where
   sizeOf        = const 4
   alignment     = sizeOf   -- not sure about this
   peek          = primReadInt32AtAddr
   poke          = primWriteInt32AtAddr

instance Storable Int64 where
   sizeOf        = const 8
   alignment     = sizeOf   -- not sure about this
   peek          = primReadInt64AtAddr
   poke          = primWriteInt64AtAddr

instance Storable (FunPtr a) where
  sizeOf    = const 4
  alignment = const 4
  peek p    = do v <- peek (castPtr p); return (castPtrToFunPtr v)
  poke p x  = do poke (castPtr p) (castFunPtrToPtr x)

---------------------------------------------------------------------------

