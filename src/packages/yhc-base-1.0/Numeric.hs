module Numeric
  (
    expt,
    FFFormat(..),
    floatToDigits,
    formatRealFloat,
    fromRat,
    lexDigits,
    nonnull,
    readDec,
    readOct,
    readHex,
    readFloat,
    readInt,
    readSigned,
    showEFloat,
    showFFloat,
    showFloat,
    showFloat,
    showGFloat,
    showInt,
    showIntAtBase,
    showHex,
    showOct,
    showBin,
    showSigned,
    Rational
  ) where

import Data.Array (Array,array,(!))
#ifndef __HADDOCK__
import Data._CharNumeric
#endif
import Data.Char
import Data.Ratio

  
-- Originally From Expt.hs  


-- Exponentiation with a cache for the most common numbers.
minExpt = 0::Int
maxExpt = 1100::Int
expt :: Integer -> Int -> Integer
expt base n =
    if base == 2 && n >= minExpt && n <= maxExpt then
        expts!n
    else
        base^n

expts :: Array Int Integer
expts = array (minExpt,maxExpt) [(n,2^n) | n <- [minExpt .. maxExpt]]

  
  
-- Originally From FFFormat.hs  

-- These are the floating point string-format styles.
-- This type is not exported from Numeric.

data FFFormat = FFExponent | FFFixed | FFGeneric
  
  
-- Originally From FloatToDigits.hs  


--
-- Based on "Printing Floating-Point Numbers Quickly and Accurately"
-- by R.G. Burger and R. K. Dybvig, in PLDI 96.
-- The version here uses a much slower logarithm estimator.  
-- It should be improved.

-- This function returns a non-empty list of digits (Ints in [0..base-1])
-- and an exponent.  In general, if
--      floatToDigits r = ([a, b, ... z], e)
-- then
--      r = 0.ab..z * base^e
-- 

floatToDigits :: (RealFloat a) => Integer -> a -> ([Int], Int)

floatToDigits _ 0 = ([], 0)
floatToDigits base x =
    let (f0, e0) = decodeFloat x
        (minExp0, _) = floatRange x
        p = floatDigits x
        b = floatRadix x
        minExp = minExp0 - p            -- the real minimum exponent
        -- Haskell requires that f be adjusted so denormalized numbers
        -- will have an impossibly low exponent.  Adjust for this.
        f :: Integer
        e :: Int
        (f, e) = let n = minExp - e0
                 in  if n > 0 then (f0 `div` (b^n), e0+n) else (f0, e0)

        (r, s, mUp, mDn) =
           if e >= 0 then
               let be = b^e in
               if f == b^(p-1) then
                   (f*be*b*2, 2*b, be*b, b)
               else
                   (f*be*2, 2, be, be)
           else
               if e > minExp && f == b^(p-1) then
                   (f*b*2, b^(-e+1)*2, b, 1)
               else
                   (f*2, b^(-e)*2, 1, 1)
        k = 
            let k0 =
                    if b==2 && base==10 then
                        -- logBase 10 2 is slightly bigger than 3/10 so
                        -- the following will err on the low side.  Ignoring
                        -- the fraction will make it err even more.
                        -- Haskell promises that p-1 <= logBase b f < p.
                        (p - 1 + e0) * 3 `div` 10
                    else
                        ceiling ((log (fromInteger (f+1)) + 
                                 fromIntegral e * log (fromInteger b)) / 
                                  log (fromInteger base))
                fixup n =
                    if n >= 0 then
                        if r + mUp <= expt base n * s then n else fixup (n+1)
                    else
                        if expt base (-n) * (r + mUp) <= s then n
                                                           else fixup (n+1)
            in  fixup k0

        gen ds rn sN mUpN mDnN =
            let (dn, rn') = (rn * base) `divMod` sN
                mUpN' = mUpN * base
                mDnN' = mDnN * base
            in  case (rn' < mDnN', rn' + mUpN' > sN) of
                (True,  False) -> dn : ds
                (False, True)  -> dn+1 : ds
                (True,  True)  -> if rn' * 2 < sN then dn : ds else dn+1 : ds
                (False, False) -> gen (dn:ds) rn' sN mUpN' mDnN'
        rds =
            if k >= 0 then
                gen [] r (s * expt base k) mUp mDn
            else
                let bk = expt base (-k)
                in  gen [] (r * bk) s (mUp * bk) (mDn * bk)
    in  (map fromIntegral (reverse rds), k)
  
  
-- Originally From FormatRealFloat.hs  


formatRealFloat :: (RealFloat a) => FFFormat -> Maybe Int -> a -> String
formatRealFloat fmt decs x 
  = s
  where 
    base = 10
    s = if isNaN x then 
            "NaN"
        else if isInfinite x then 
            if x < 0 then "-Infinity" else "Infinity"
        else if x < 0 || isNegativeZero x then 
            '-' : doFmt fmt (floatToDigits (toInteger base) (-x))
        else 
            doFmt fmt (floatToDigits (toInteger base) x)
    
    mk0 "" = "0"            -- Used to ensure we print 34.0, not 34.
    mk0 s  = s              -- and 0.34 not .34
    
    mkdot0 "" = ""          -- Used to ensure we print 34, not 34.
    mkdot0 s  = '.' : s     -- when the format specifies no digits
                            -- after the decimal point
    
    doFmt fmt (is, e)
      = let 
           ds = map intToDigit is
        in  
        case fmt of
          FFGeneric -> 
              doFmt (if e < 0 || e > 7 then FFExponent else FFFixed)
                    (is, e)
          FFExponent ->
            case decs of
              Nothing ->
                case ds of
                   []     -> "0.0e0"
                   [d]    -> d : ".0e" ++ show (e-1)
                   (d:ds) -> d : '.' : ds ++ 'e':show (e-1)
       
              Just dec ->
                let dec' = max dec 1 in
                case is of
                  [] -> '0':'.':take dec' (repeat '0') ++ "e0"
                  _ ->
                    let (ei, is') = roundTo base (dec'+1) is
                        (d:ds) = map intToDigit
                                     (if ei > 0 then init is' else is')
                    in d:'.':ds  ++ "e" ++ show (e-1+ei)
       

          FFFixed ->
            case decs of
        --     Nothing ->
        --         let f 0 s ds = mk0 s ++ "." ++ mk0 ds
        --             f n s "" = f (n-1) (s++"0") ""
        --             f n s (d:ds) = f (n-1) (s++[d]) ds
        --         in f e "" ds

               Nothing      -- always prints a decimal point
                 | e > 0     -> take e (ds ++ repeat '0')
                                ++ '.' : mk0 (drop e ds)
                 | otherwise -> "0." ++ mk0 (replicate (-e) '0' ++ ds)
              
               Just dec ->  -- print decimal point iff dec > 0
                 let dec' = max dec 0 in
                 if e >= 0 then
                   let (ei, is') = roundTo base (dec' + e) is
                       (ls, rs)  = splitAt (e+ei) 
                                              (map intToDigit is')
                   in  mk0 ls ++ mkdot0 rs
                 else
                   let (ei, is') = roundTo base dec' 
                                           (replicate (-e) 0 ++ is)
                       (d:ds) = map intToDigit 
                                    (if ei > 0 then is' else 0:is')
                   in  d : mkdot0 ds

roundTo :: Int -> Int -> [Int] -> (Int, [Int])
roundTo base d is = case f d is of
                (0, is) -> (0, is)
                (1, is) -> (1, 1 : is)
  where b2 = base `div` 2
        f n [] = (0, replicate n 0)
        f 0 (i:_) = (if i >= b2 then 1 else 0, [])
        f d (i:is) = 
            let (c, ds) = f (d-1) is
                i' = c + i
            in  if i' == base then (1, 0:ds) else (0, i':ds)


  
  
-- Originally From FromRat.hs  


-- This converts a rational to a floating.  This should be used in the
-- Fractional instances of Float and Double.

fromRat :: (RealFloat a) => Rational -> a
fromRat x = 
    if x == 0 then encodeFloat 0 0              -- Handle exceptional cases
    else if x < 0 then - fromRat' (-x)          -- first.
    else fromRat' x

-- Conversion process:
-- Scale the rational number by the RealFloat base until
-- it lies in the range of the mantissa (as used by decodeFloat/encodeFloat).
-- Then round the rational to an Integer and encode it with the exponent
-- that we got from the scaling.
-- To speed up the scaling process we compute the log2 of the number to get
-- a first guess of the exponent.
fromRat' :: (RealFloat a) => Rational -> a
fromRat' x = r
  where b = floatRadix r
        p = floatDigits r
        minExp = (fst (floatRange r)) - p       -- the real minimum exponent
        xMin = toRational (expt b (p-1))
        xMax = toRational (expt b p)
        p0 = (integerLogBase b (numerator x) -
              integerLogBase b (denominator x) - p) `max` minExp
        f = if p0 < 0 then 1 % expt b (-p0) else expt b p0 % 1
        xp = scaleRat (toRational b) minExp xMin xMax p0 (x / f)
        r = encodeFloat (round (fst xp)) (snd xp)

-- Scale x until xMin <= x < xMax, or p (the exponent) <= minExp.
scaleRat :: Rational -> Int -> Rational -> Rational -> 
             Int -> Rational -> (Rational, Int)
scaleRat b minExp xMin xMax p x =
    if p <= minExp then
        (x, p)
    else if x >= xMax then
        scaleRat b minExp xMin xMax (p+1) (x/b)
    else if x < xMin  then
        scaleRat b minExp xMin xMax (p-1) (x*b)
    else
        (x, p)

-- Compute the (floor of the) log of i in base b.
-- Simplest way would be just divide i by b until it's smaller then b,
-- but that would be very slow!  We are just slightly more clever.
integerLogBase :: Integer -> Integer -> Int
integerLogBase b i =
     if i < b then
        0
     else
        -- Try squaring the base first to cut down the number of divisions.
        let l = 2 * integerLogBase (b*b) i
            doDiv :: Integer -> Int -> Int
            doDiv i l = if i < b then l else doDiv (i `div` b) (l+1)
        in  doDiv (i `div` (b^l)) l
  
  
-- Originally From LexDigits.hs  


lexDigits       :: ReadS String 

lexDigits       =  nonnull isDigit
 
  
  
-- Originally From ReadFloat.hs  

--import RatioCon
--import Fractional_Ratio

readFloat:: (RealFrac a) => ReadS a

readFloat r = [(fromRational ((n%1)*10^^(k-d)), t) | (n,d,s) <- readFix r,
                             (k,t)   <- readExp s]  ++
              [ (0/0, t) | ("NaN",t)      <- lex r] ++
              [ (1/0, t) | ("Infinity",t) <- lex r]
              where readFix r = [(read (ds++ds'), length ds', t)
                                | (ds,s)  <- lexDigits r,
                                (ds',t) <- lexDot s ]

                    lexDot ('.':s)   = lexDigits s
                    lexDot s         = [("",s)]

                    readExp (e:s) | e `elem` "eE" = readExp' s
                    readExp s             = [(0,s)]

                    readExp' ('-':s) = [(negate k,t) | (k,t) <- readDec s]
                    readExp' ('+':s) = readDec s
                    readExp' s       = readDec s

  
  
-- Originally From ReadSigned.hs  

readSigned:: (Real a) => ReadS a -> ReadS a

readSigned readPos = readParen False read'
             where read' r  = read'' r ++
                      [(negate x,t) | ("-",s) <- lex r,
                              (x,t)   <- read'' s]
                   read'' r = [(n,s)  | (str,s) <- lex r,
                               (n,"")  <- readPos str]
  
  
-- Originally From ShowEFloat.hs  


showEFloat     :: (RealFloat a) => Maybe Int -> a -> ShowS
showEFloat d x =  showString (formatRealFloat FFExponent d x)
  
  
-- Originally From ShowFFloat.hs  


showFFloat     :: (RealFloat a) => Maybe Int -> a -> ShowS
showFFloat d x =  showString (formatRealFloat FFFixed d x)
  
  
-- Originally From ShowFloat.hs  


showFloat :: (RealFloat a) => a -> ShowS
showFloat = showGFloat Nothing

{-
-- All the code below is from a earlier version of the Numeric library.
-- It has now be replaced by the code above.


-- The number of decimal digits m below is chosen to guarantee 
-- read (show x) = x.  See
--  Matula, D. W.  A formalization of floating-point numeric base
--  conversion.  IEEE Transactions on Computers C-19, 8 (1970 August),
--  681-692.

showFloat:: (RealFloat a) => a -> ShowS
showFloat x = if x < 0 then showChar '-' . showFloat' (negate x) else showFloat' x

showFloat' x =
{- !!! Lennart use the following lines, don't know if they are standard !!!
    if isNaN x then showString "NaN" else
    if isInfinite x then showString "Infinity" else
-}
    if x == 0 then showString ("0." ++ take (m-1) (repeat '0'))
          else if e >= m-1 || e < 0 then showSci else showFix
    where
    showFix = showString whole . showChar '.' . showString frac
          where (whole,frac) = splitAt (e+1) (show sig)
    showSci = showChar d . showChar '.' . showString frac
              . showChar 'e' . shows e
              where (d:frac) = show sig

    (m, sig, e) = if b == 10 then (w,   s,   n+w-1)
                 else (m', sig', e'   )
    m' :: Int
    m'      = ceiling
              ((fromIntegral w * log (fromInteger b)) / log (fromInteger 10) :: Double)
          + 1

    (sig', e')  = if      sig1 >= (10::Integer)^m' then ((round (t/10))::Integer, e1+1)
          else if sig1 <  (10::Integer)^(m'-1) then ((round (t*10))::Integer, e1-1)
                        else (sig1,     e1  )
    sig1 :: Integer
    sig1    = round t
    t    :: Rational
    t           = (s%1) * (b%1)^^n * 10^^(m'-e1-1)
    e1   :: Int
    e1      = floor (logBase 10 x)
    (s, n)  = decodeFloat x
    b       = floatRadix x
    w       = floatDigits x

-}
  
  
-- Originally From ShowGFloat.hs  


showGFloat     :: (RealFloat a) => Maybe Int -> a -> ShowS
showGFloat d x =  showString (formatRealFloat FFGeneric d x)
  
  
-- Originally From ShowInt.hs  


showInt :: (Integral a) => a -> ShowS
showInt = showIntAtBase 10 intToDigit

  
  
-- Originally From ShowIntBase.hs  


showIntAtBase :: Integral a => a -> (Int->Char) -> a -> ShowS
showIntAtBase base intToDig n r
  | n < 0   = error "Numeric.showIntAtBase: can't show negative numbers"
  | otherwise =
      let (n',d) = quotRem n base
          r'     = intToDig (fromIntegral d) : r
      in if n' == 0 then r' else showIntAtBase base intToDig n' r'

showHex, showOct, showBin :: Integral a => a -> ShowS
showHex = showIntAtBase 16 intToDigit
showOct = showIntAtBase 8 intToDigit
showBin = showIntAtBase 2 intToDigit
  
  
-- Originally From ShowSigned.hs  

showSigned:: (Real a) => (a -> ShowS) -> Int -> a -> ShowS
showSigned showPos p x = 
     if x < 0 then showParen (p > 6)
         (showChar '-' . showPos (negate x))
     else 
          showPos x
