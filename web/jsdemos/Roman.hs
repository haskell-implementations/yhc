module Roman where

import Numeric (readDec)
import Maybe   (fromJust, fromMaybe)
import Char    (toUpper)

{- Roman Numerals.  You can use toRoman and fromRoman directly
-- if you wish.  You can also compile this module as a program and
-- use it at the commandline under the names toRoman or fromRoman,
-- depending on the sense you want the conversion.  Note, there isn't
-- any error-checking on the input strings, and we use limited precision
-- Ints because surely no-one wants to play with roman numerals any
-- larger than that!

-- Author: Malcolm.Wallace@cs.york.ac.uk, 29 July 1999
-}

toRoman   :: Int -> String
fromRoman :: String -> Int


-- Each numeral has a decimal value.
numerals = [ ('I',   1), ('V',   5), ('X',  10), ('L',  50),
             ('C', 100), ('D', 500), ('M',1000) ]

-- For each numeral, there is a single permitted prefix digit for subtraction.
subnums  = [ ('V','I'),  ('X','I'),  ('L','X'),
             ('C','X'),  ('D','C'),  ('M','C') ]

-- Traverse the numeral list with an accumulator consisting of the
-- string built so far (in reverse order) and the remaining value to be
-- converted.
toRoman n  = (reverse . snd) (foldr toNumeral (n,"") numerals)

-- Each numeral could potentially appear many times (case 1), and we must
-- also handle (case 2) where a numeral *nearly* fits so we use a subtractive
-- prefix.
toNumeral st@(rdigit, base) (n,s)
  | n >= base    = toNumeral st (n-base, rdigit:s)
  | n+k >= base  = (n-base+k, rdigit:tdigit:s)
  | otherwise    = (n,s)
  where tdigit = fromMaybe '\0' (lookup rdigit subnums)
        k      = fromMaybe  0   (lookup tdigit numerals)



-- The inverse is pretty straightforward by comparison.  First, divide
-- up the string into chunks of identical letters, and add those together
-- (maxmunch).  Then accumulate these from the right - an intermediate
-- letter-sum which is less than the value already accumulated means it
-- must be a prefix subtraction (fromNumeral) rather than an addition.

fromRoman = foldr fromNumeral 0 . maxmunch . map toUpper
fromNumeral x y
  | x < y  = y-x
  | x > y  = y+x
maxmunch "" = []
maxmunch string@(x:_) =
  let (these,those) = span (x==) string
      length [] = 0
      length (x:xs) = 1 + length xs
  in fromJust (lookup x numerals) * length these : maxmunch those



-- Now just some tidying up so we can call the program from the
-- commandline.

safeRead s =
  case readDec s of
    [] -> 0
    ((n,_):_) -> n



