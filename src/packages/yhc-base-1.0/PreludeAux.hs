-- because 'Prelude' is a bit special some things won't compile there,
-- so they've just been moved to here instead.

module PreludeAux(
   _doubleToRational,
   _doubleFromRational,
   _floatToRational,
   _floatFromRational,
   _showsErrNo,
   _fromEnumErrNo
  )where

import Data.Ratio
import YHC.ErrNo

_doubleToRational x =       
    case decodeFloat x of (m,n) -> (m%1)*(b%1)^^n
            where b     = floatRadix  x

_doubleFromRational x =
      let f ex = let y :: Double
                     y  = encodeFloat (round (x * (1 % bd) ^^ ex)) ex
                     e' = snd (decodeFloat y)
                     bd = floatRadix x'
                 in if e' == ex then y else f e'
          e    = snd (decodeFloat (fromInteger (numerator x) `asTypeOf` x'
                                            / fromInteger (denominator x)))
          x'   = f e
      in x'


_floatToRational x =
    case decodeFloat x of (m,n) -> (m%1)*(bf%1)^^n
            where bf     = floatRadix  x


_floatFromRational x = x0
      where x0    = ff ef
            ff ef = if ef' == ef then yf else ff ef'
                   where yf :: Float
                         yf      = encodeFloat (round (x * (1 % bf) ^^ ef)) ef
                         (_,ef') = decodeFloat yf
                         bf      = floatRadix x0
            (_,ef) = decodeFloat (fromInteger (numerator x) `asTypeOf` x0
                                            / fromInteger (denominator x))

_showsErrNo :: ErrNo -> ShowS
_showsErrNo e = shows e

_fromEnumErrNo :: ErrNo -> Int
_fromEnumErrNo e = fromEnum e