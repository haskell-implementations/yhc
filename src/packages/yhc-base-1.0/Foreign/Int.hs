{-# OPTIONS_YHC --cpp #-}
module Foreign.Int
  -- all types are abstract and instances of:
  -- Num, Bounded, Real, Integral, Ix, Enum, Read, Show
  ( Int8
  , Int16
  , Int32
  , Int64
  ) where

{- Note explicit braces and semicolons here - layout is corrupted by cpp. -}

{import Numeric (readSigned,readDec,showSigned,showInt)
;import Data.Ix
;import YHC.Primitive


#define INT_TYPE(T,BS,LB,UB)  \
; INSTANCE_EQ(T)    \
; INSTANCE_ORD(T)   \
; INSTANCE_NUM(T)   \
; INSTANCE_BOUNDED(T,LB,UB) \
; INSTANCE_REAL(T)    \
; INSTANCE_INTEGRAL(T)    \
; INSTANCE_IX(T)    \
; INSTANCE_ENUM(T)    \
; INSTANCE_READ(T)    \
; INSTANCE_SHOW(T)

#define INSTANCE_EQ(T)  \
; instance Eq T where \
    { (==) = primEq##T  \
    }

#define INSTANCE_ORD(T)   \
; instance Ord T where    \
    { (<)  = primLt##T    \
    ; (<=) = primLe##T    \
    ; (>)  = primGt##T    \
    ; (>=) = primGe##T    \
    }

#define INSTANCE_NUM(T)     \
; instance Num T where      \
    { (+) = primAdd##T      \
    ; (-) = primSub##T      \
    ; (*) = primMul##T      \
    ; abs = primAbs##T      \
    ; signum = primSignum##T    \
    ; fromInteger = prim##T##FromInteger\
    }

#define INSTANCE_BOUNDED(T,LB,UB) \
; instance Bounded T where    \
    { minBound = fromInteger LB   \
    ; maxBound = fromInteger UB   \
    }

#define INSTANCE_REAL(T)    \
; instance Real T where     \
    { toRational i = toInteger i % 1  \
    }

#define INSTANCE_INTEGRAL(T)    \
; instance Integral T where   \
    { quot = primQuot##T    \
    ; rem  = primRem##T     \
    ; toInteger = prim##T##ToInteger  \
    }

#define INSTANCE_IX(T)      \
; instance Ix T where     \
    { range (m,n) = [m..n]    \
    ; index b@(m,n) i     \
      | inRange b i = fromIntegral (i-m)      \
      | True        = error "Ix.index: Index out of range." \
    ; inRange (m,n) i    = m <= i && i <= n     \
    }

#define INSTANCE_ENUM(T)    \
; instance Enum T where     \
    { toEnum = primToEnum##T    \
    ; fromEnum = primFromEnum##T  \
    ; enumFrom n = n : enumFrom (n+1) \
    ; enumFromThen m n = m : enumFromThen n (2*n-m) \
    }

#define INSTANCE_READ(T)    \
; instance Read T where     \
    { readsPrec p = readSigned readDec  \
    }

#define INSTANCE_SHOW(T)    \
; instance Show T where     \
    { showsPrec p = showSigned showInt p  \
    }


INT_TYPE(Int8,8,(-128),127)
INT_TYPE(Int16,16,(-32768),32767)
INT_TYPE(Int32,32,(-2147483648),2147483647)
INT_TYPE(Int64,64,(-9223372036854775808),9223372036854775807)

}
