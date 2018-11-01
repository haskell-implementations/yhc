-- An overlay for Prelude and others to use with SSA intermediate form.
#ifndef __HADDOCK__
module SSAOverlay where

import UnsafeJS

-- To substitute Prelude.error

global_Prelude'_error :: String -> a

global_Prelude'_error a = unsafeJS "throw('E ' + exprEval(a).toString()); return undefined;"

-- To substitute Prelude.seq

-- global_Prelude'_seq :: a -> b -> b

-- global_Prelude'_seq a b = unsafeJS
--   "exprEval(a); return b;"

-- To substitute Integer primitives

global_YHC'_Primitive'_primIntegerQuot a b = unsafeJS 
  "(function(x,y){return (x - (x % y))/y;})(exprEval(a),exprEval(b));"

global_YHC'_Primitive'_primIntegerRem a b = unsafeJS "return exprEval(a) % exprEval(b);"

global_YHC'_Primitive'_primIntegerQuotRem :: Integer -> Integer -> (Integer, Integer)

global_YHC'_Primitive'_primIntegerQuotRem a b =
  let q = a `quot` b
      r = a `rem` b in
  (q, r)

global_YHC'_Primitive'_primIntegerAdd a b = unsafeJS "return exprEval(a) + exprEval(b);"

global_YHC'_Primitive'_primIntegerSub a b = unsafeJS "return exprEval(a) - exprEval(b);"

global_YHC'_Primitive'_primIntegerMul a b = unsafeJS "return exprEval(a) * exprEval(b);"

global_YHC'_Primitive'_primIntegerNeg a = unsafeJS "return 0 - Number(exprEval(a));"

global_YHC'_Primitive'_primIntegerEq a b = unsafeJS
  "return Number(exprEval(a)) === Number(exprEval(b));"

global_YHC'_Primitive'_primIntegerNe a b = unsafeJS 
  "return Number(exprEval(a)) !== Number(exprEval(b));"

global_YHC'_Primitive'_primIntegerGe a b = unsafeJS 
  "return Number(exprEval(a)) >= Number(exprEval(b));"

global_YHC'_Primitive'_primIntegerGt a b = unsafeJS
  "return Number(exprEval(a)) > Number(exprEval(b));"

global_YHC'_Primitive'_primIntegerLe a b = unsafeJS
  "return Number(exprEval(a)) <= Number(exprEval(b));"

global_YHC'_Primitive'_primIntegerLt a b = unsafeJS
  "return Number(exprEval(a)) < Number(exprEval(b));"

global_YHC'_Primitive'_primIntegerFromInt a = unsafeJS "return a;"

global_YHC'_Primitive'_primIntFromInteger a = unsafeJS "return a;"

global_YHC'_Primitive'_primIntAbs a = unsafeJS "return Math.abs(exprEval(a));"

global_YHC'_Primitive'_primIntSignum a = unsafeJS
  "var ea = exprEval(a); if (ea>0) return 1; else if (ea<0) return -1; else return 0;"

global_Prelude'_Prelude'_Num'_Prelude'_Int'_negate a = unsafeJS "return 0 - Number(exprEval(a));"

-- instance Show Int where ... fix character string generation
-- have Javascript host do that. Also do not refer to minBound

global_Prelude'_Prelude'_Show'_Prelude'_Int'_showsPrec :: Int -> Int -> ShowS
global_Prelude'_Prelude'_Show'_Prelude'_Int'_showsPrec p x =
  if x < 0 then showParen (p > 6)
    (showChar '-' .  showPosInt (negate x))
  else
    showPosInt x
  where showInt :: Int -> String
        showInt a = unsafeJS "return Number(exprEval(a)).toString();"
        showPosInt :: Int -> String -> String
        showPosInt a b = showInt a ++ b

#endif

