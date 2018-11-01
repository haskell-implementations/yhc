{-# OPTIONS_YHC --cpp #-}
{-# OPTIONS_COMPILE --cpp #-}
module Foreign.C.Types
	( -- Integral types, instances of: Eq, Ord, Num, Read, Show, Enum,
	  -- Typeable, Storable, Bounded, Real, Integral, Bits
	  CChar(..),    CSChar(..),  CUChar(..)
	, CShort(..),   CUShort(..), CInt(..),    CUInt(..)
	, CLong(..),    CULong(..),  CLLong(..),  CULLong(..)

	  -- Floating types, instances of: Eq, Ord, Num, Read, Show, Enum,
	  -- Typeable, Storable, Real, Fractional, Floating, RealFrac, RealFloat
	, CFloat(..),   CDouble(..), CLDouble(..)

         -- Integral types, instances of: Eq, Ord, Num, Read, Show, Enum,
         -- Storable, Bounded, Real, Integral
        , CPtrdiff(..), CSize(..), CWchar(..), CSigAtomic(..)

        -- Numeric types, instances of: Eq, Ord, Num, Read, Show, Enum, Storable
        , CClock(..),   CTime(..)
        , CFile,        CFpos,     CJmpBuf

  -- C99 types which are still missing include:
  -- intptr_t, uintptr_t, intmax_t, uintmax_t, wint_t, wctrans_t, wctype_t
	) where

import YHC.Primitive(unsafeCoerce)
import Foreign.Int	( Int8,  Int16,  Int32,  Int64  )
import Foreign.Word	( Word8, Word16, Word32, Word64 )
import Foreign.Storable	( Storable(..) )
-- import Data.Bits( Bits(..) )
-- import NHC.SizedTypes
import Control.Monad	( liftM )
import Foreign.Ptr	( castPtr )

#include "Types.h"

INTEGRAL_TYPE(CChar,Int8)
INTEGRAL_TYPE(CSChar,Int8)
INTEGRAL_TYPE(CUChar,Word8)
INTEGRAL_TYPE(CShort,Int16)
INTEGRAL_TYPE(CUShort,Word16)
INTEGRAL_TYPE(CInt,Int)
INTEGRAL_TYPE(CUInt,Word32)
INTEGRAL_TYPE(CLong,Int32)
INTEGRAL_TYPE(CULong,Word32)
INTEGRAL_TYPE(CLLong,Int64)
INTEGRAL_TYPE(CULLong,Word64)

FLOATING_TYPE(CFloat,Float)
FLOATING_TYPE(CDouble,Double)
-- HACK: Currently no long double in the FFI, so we simply re-use double
FLOATING_TYPE(CLDouble,Double)

-- So far, we just make a best guess at these types for most 32-bit machines.
-- Should really be auto-configured.

INTEGRAL_TYPE(CPtrdiff,CInt)
INTEGRAL_TYPE(CWchar,CInt)
INTEGRAL_TYPE(CSigAtomic,CInt)
INTEGRAL_TYPE(CSize,CInt)

ARITHMETIC_TYPE(CClock,CUInt)
ARITHMETIC_TYPE(CTime,CUInt)

-- Not sure what these are for??
data CFile   = CFile		-- ??
data CFpos   = CFpos		-- ??
data CJmpBuf = CJmpBuf		-- ??
