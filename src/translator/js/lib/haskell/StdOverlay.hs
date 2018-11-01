-- A "Standard" overlay for Prelude and others
#ifndef __HADDOCK__

module StdOverlay where

import UnsafeJS
import Data.Char
import Numeric

-- To substitute DOM.Level2.XMLHttpRequest;sendString'

global_DOM'_Level2'_XMLHttpRequest''sendString' a b = unsafeJS
  "var x = exprEval(b);return (exprEval(a)).send(x + \"\");"

-- To substitute Prelude.error

global_Prelude''error :: String -> a

global_Prelude''error a = unsafeJS "throw('E ' + exprEval(a).toString()); return undefined;"

-- To substitute Integer primitives

global_YHC'_Primitive''primIntegerQuot a b = unsafeJS 
  "(function(x,y){return (x - (x % y))/y;})(exprEval(a),exprEval(b));"

global_YHC'_Primitive''primIntegerRem a b = unsafeJS "return exprEval(a) % exprEval(b);"

global_YHC'_Primitive''primIntegerQuotRem :: Integer -> Integer -> (Integer, Integer)

global_YHC'_Primitive''primIntegerQuotRem a b =
  let q = a `quot` b
      r = a `rem` b in
  (q, r)

global_YHC'_Primitive''primIntegerAdd a b = unsafeJS "return exprEval(a) + exprEval(b);"

global_YHC'_Primitive''primIntegerSub a b = unsafeJS "return exprEval(a) - exprEval(b);"

global_YHC'_Primitive''primIntegerMul a b = unsafeJS "return exprEval(a) * exprEval(b);"

global_YHC'_Primitive''primIntegerNeg a = unsafeJS "return 0 - Number(exprEval(a));"

global_YHC'_Primitive''primIntegerEq a b = unsafeJS
  "return Number(exprEval(a)) === Number(exprEval(b));"

global_YHC'_Primitive''primIntegerNe a b = unsafeJS 
  "return Number(exprEval(a)) !== Number(exprEval(b));"

global_YHC'_Primitive''primIntegerGe a b = unsafeJS 
  "return Number(exprEval(a)) >= Number(exprEval(b));"

global_YHC'_Primitive''primIntegerGt a b = unsafeJS
  "return Number(exprEval(a)) > Number(exprEval(b));"

global_YHC'_Primitive''primIntegerLe a b = unsafeJS
  "return Number(exprEval(a)) <= Number(exprEval(b));"

global_YHC'_Primitive''primIntegerLt a b = unsafeJS
  "return Number(exprEval(a)) < Number(exprEval(b));"

global_YHC'_Primitive''primIntegerFromInt a = unsafeJS "return a;"

global_YHC'_Primitive''primIntFromInteger a = unsafeJS "return a;"

global_YHC'_Primitive''primIntAbs a = unsafeJS "return Math.abs(exprEval(a));"

global_YHC'_Primitive''primIntSignum a = unsafeJS
  "var ea = exprEval(a); if (ea>0) return 1; else if (ea<0) return -1; else return 0;"

global_YHC'_Primitive''primThrow a = unsafeJS "throw('Y ' + exprEval(a).toSource()); return undefined;"

-- Instance Class Type: Type goes first, Class second

-- instance Num Int where negate = ...

global_Prelude''Prelude'_Int''Prelude'_Num''negate a = unsafeJS "return 0 - Number(exprEval(a));"

-- instance Show Int where ... fix character string generation
-- have Javascript host do that. Also do not refer to minBound

global_Prelude''Prelude'_Int''Prelude'_Show''showsPrec :: Int -> Int -> ShowS
global_Prelude''Prelude'_Int''Prelude'_Show''showsPrec p x =
  if x < 0 then showParen (p > 6)
    (showChar '-' .  showPosInt (negate x))
  else
    showPosInt x
  where showInt :: Int -> String
        showInt a = unsafeJS "return Number(exprEval(a)).toString();"
        showPosInt :: Int -> String -> String
        showPosInt a b = showInt a ++ b

-- Do the same for Integer (Int and Integer are the same thing in Javascript)

global_Prelude''Prelude'_Integer''Prelude'_Show''showsPrec :: Int -> Integer -> ShowS
global_Prelude''Prelude'_Integer''Prelude'_Show''showsPrec p x = 
  global_Prelude''Prelude'_Int''Prelude'_Show''showsPrec p (fromIntegral x)


ord :: Char -> Int
ord = fromEnum

-- Replace toUpper, toLower with Unicode-enabled versions

global_Data'_Char''toUpper = tou . ord
  where tou a = unsafeJS "return mkChar(uToUpper(exprEval(a)));"

global_Data'_Char''toLower = tol . ord
  where tol a = unsafeJS "return mkChar(uToLower(exprEval(a)));"

-- Replace other categorizing functions

global_Data'_Char''isAlpha = f . ord
  where f a = unsafeJS "return uIsAlpha(exprEval(a));"

global_Data'_Char''isAlphaNum = f . ord
  where f a = unsafeJS "return uIsAlNum(exprEval(a));"

global_Data'_Char''isControl = f . ord
  where f a = unsafeJS "return uIsControl(exprEval(a));"

global_Data'_Char''isLower = f . ord
  where f a = unsafeJS "return uIsLower(exprEval(a));"

global_Data'_Char''isUpper = f . ord
  where f a = unsafeJS "return uIsUpper(exprEval(a));"

global_Data'_Char''isSpace = f . ord
  where f a = unsafeJS "return uIsSpace(exprEval(a));"

global_Data'_Char''isPrint = f . ord
  where f a = unsafeJS "return uIsPrint(exprEval(a));"

global_Prelude''fromIntegral :: (Integral a, Num b) => a -> b

global_Prelude''fromIntegral x = f x where f a = unsafeJS "return a;"

-- Replacement for Numeric.readFloat

global_Numeric''readFloat :: (RealFrac a) => ReadS a

global_Numeric''readFloat r = [(mkFloat n k d, t) | (n,d,s) <- readFix r,
                             (k,t)   <- readExp s]  ++
              [ (0/0, t) | ("NaN",t)      <- lex r] ++
              [ (1/0, t) | ("Infinity",t) <- lex r]
              where readFix r = [(read (ds++ds'), length ds', t)
                                | (ds,s)  <- lexDigits r,
                                (ds',t) <- lexDot s ]

                    lexDot ('.':s)   = lexDigits s
                    lexDot s         = [("",s)]

                    readExp (e:s) | e == 'e' || e == 'E' = readExp' s
                    readExp s             = [(0,s)]

                    readExp' ('-':s) = [(negate k,t) | (k,t) <- readDec s]
                    readExp' ('+':s) = readDec s
                    readExp' s       = readDec s
                    mkFloat a b c = unsafeJS
                      "return exprEval(a) * Math.pow(10, (exprEval(b) - exprEval(c)));"



global_Numeric''showFloat _ f s = 
  (shf f) ++ s where shf a = unsafeJS "return Number(exprEval(a)).toString();"

global_Numeric''showDouble = global_Numeric''showFloat

primStub a = unsafeJS "throw 'N: ' + a + ' primitive not implemented'; return undefined;"

-- Float/double primitive stubs

global_YHC'_Primitive''primEncodeFloat = primStub "primEncodeFloat"
global_YHC'_Primitive''primDecodeFloat = primStub "primDecodeFloat"
global_YHC'_Primitive''primEncodeDouble = primStub "primEncodeDouble"
global_YHC'_Primitive''primDecodeDouble = primStub "primDecodeDouble"


-- Float/double primitives

global_YHC'_Primitive''primFloatPow a b = unsafeJS
  "return Math.pow(exprEval(a), exprEval(b));"

global_YHC'_Primitive''primDoublePow = global_YHC'_Primitive''primFloatPow

global_YHC'_Primitive''primFloatACos a = unsafeJS
  "return Math.acos(exprEval(a));"

global_YHC'_Primitive''primDoubleACos = global_YHC'_Primitive''primFloatACos

global_YHC'_Primitive''primFloatASin a = unsafeJS
  "return Math.asin(exprEval(a));"

global_YHC'_Primitive''primDoubleASin = global_YHC'_Primitive''primFloatASin

global_YHC'_Primitive''primFloatCos a = unsafeJS
  "return Math.cos(exprEval(a));"

global_YHC'_Primitive''primDoubleCos = global_YHC'_Primitive''primFloatCos

global_YHC'_Primitive''primFloatSin a = unsafeJS
  "return Math.sin(exprEval(a));"

global_YHC'_Primitive''primDoubleSin = global_YHC'_Primitive''primFloatSin

global_YHC'_Primitive''primFloatTan a = unsafeJS
  "return Math.tan(exprEval(a));"

global_YHC'_Primitive''primDoubleTan = global_YHC'_Primitive''primFloatTan

global_YHC'_Primitive''primFloatATan a = unsafeJS
  "return Math.atan(exprEval(a));"

global_YHC'_Primitive''primDoubleATan = global_YHC'_Primitive''primFloatATan

global_YHC'_Primitive''primFloatLog a = unsafeJS
  "return Math.log(exprEval(a));"

global_YHC'_Primitive''primDoubleLog = global_YHC'_Primitive''primFloatLog

global_YHC'_Primitive''primFloatSqrt a = unsafeJS
  "return Math.sqrt(exprEval(a));"

global_YHC'_Primitive''primDoubleSqrt = global_YHC'_Primitive''primFloatSqrt

global_YHC'_Primitive''primFloatExp a = unsafeJS
  "return Math.exp(exprEval(a));"

global_YHC'_Primitive''primDoubleExp = global_YHC'_Primitive''primFloatExp

global_YHC'_Primitive''primFloatAbs a = unsafeJS
  "return Math.abs(exprEval(a));"

global_YHC'_Primitive''primDoubleAbs = global_YHC'_Primitive''primFloatAbs

global_YHC'_Primitive''primFloatSignum a = unsafeJS
  "return primSignum(exprEval(a));"

global_YHC'_Primitive''primDoubleSignum = global_YHC'_Primitive''primFloatSignum

global_YHC'_Primitive''primFloatFromInteger a = unsafeJS
  "return exprEval(a);"

global_YHC'_Primitive''primDoubleFromInteger = global_YHC'_Primitive''primFloatFromInteger

-- Other ad-hoc substitutes based on profiling observations

-- Prelude;||

global_Prelude'''pip'pip a b = unsafeJS "return(exprEval(a)._t)?a:b;"

-- Prelude.id

global_Prelude''id a = unsafeJS "return a;"

-- This implementation of Prelude.any translates into shorter and faster
-- Javascript bevause its original version is too short and gets too much
-- stuff inlined.

global_Prelude''any :: (a -> Bool) -> [a] -> Bool
global_Prelude''any _ [] = False
global_Prelude''any f (x:xs) = case f x of
  True -> True
  _ -> global_Prelude''any f xs


-- This implementation of Prelude.lex (ab)uses the fact that string
-- literals are compiled directly into Javascript strings, so the elem
-- function can be specialized and replaced with Javascript indexOf.

global_Prelude''lex :: ReadS String

global_Prelude''lex ""                  = [("","")]
global_Prelude''lex (c:s) | isSpace c   = lex (dropWhile isSpace s)
global_Prelude''lex ('\'':s)            = [('\'':ch++"'", t) | (ch,'\'':t)  <- lexLitChar s,
                                               ch /= "'"                ]
global_Prelude''lex ('"':s)             = [('"':str, t)      | (str,t) <- lexString s]
                          where
                          lexString ('"':s) = [("\"",s)]
                          lexString s = [(ch++str, u)
                                                | (ch,t)  <- lexStrItem s,
                                                  (str,u) <- lexString t  ]

                          lexStrItem ('\\':'&':s) = [("\\&",s)]
                          lexStrItem ('\\':c:s) | isSpace c
                              = [("\\&",t) | '\\':t <- [dropWhile isSpace s]]
                          lexStrItem s            = lexLitChar s

global_Prelude''lex (c:s) | isSingle c  = [([c],s)]
          | isSym c     = [(c:sym,t)         | (sym,t) <- [span isSym s]]
          | isIdInit c  = [(c:nam,t)         | (nam,t) <- [span isIdChar s]]
          | isDigit c   = [(c:ds++fe,t)      | (ds,s)  <- [span isDigit s],
                                               (fe,t)  <- lexFracExp s     ]
          | otherwise   = []    -- bad character
                where
                isSingle c  =  c `fastElem` singles
                singles     =  ",;()[]{}`"
                isSym c     =  c `fastElem` syms
                syms        =  "!@#$%&*+./<=>?\\^|:-~"
                isIdInit c  =  isAlpha c || c == '_'
                isIdChar c  =  isAlphaNum c || c == '_' || c == '\''

                lexFracExp ('.':c:s) | isDigit c
                                   = [('.':ds++e,u) | (ds,t) <- lexDigits (c:s),
                                                      (e,u)  <- lexExp t    ]
                lexFracExp s       = lexExp s

                lexExp (e:s) | e == 'e' || e == 'E'
                         = [(e:c:ds,u) | (c:t)  <- [s], c == '+' || c == '-',
                                                   (ds,u) <- lexDigits t] ++
                           [(e:ds,t)   | (ds,t) <- lexDigits s]
                lexExp s = [("",s)]
                fastElem a b = unsafeJS "return (b.indexOf(exprEval(a))>=0);"


#endif

