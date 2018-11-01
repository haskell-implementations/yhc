-----------------------------------------------------------------------------
-- |
-- Module      :  UnsafeJS
-- Copyright   :  (c) Dmitry Golubovsky, 2007       
-- License     :  BSD-style
-- 
-- Maintainer  :  Dmitry Golubovsky <golubovsky@gmail.com>
-- Stability   :  stable
-- Portability :  non-portable (needs Yhc Javascript backend)
--
-- Unsafe Operations on Javascript Objects
--
-------------------------------------------------------------------------------

module UnsafeJS (
-- * Direct Interface with Javascript
-- $unsafejs
  unsafeJS,
-- * Unsafe Casts
  unsafeToNum, 
  unsafeToString,
  unsafeToSelf,
-- * Unsafe Access to Javascript Objects Properties
-- $unsafeprop
  unsafeGetProperty,
  unsafeSetProperty,
  unsafeCheckProperty,
  unsafeIsNull,
  unsafeIsString,
-- * Exception Handling
  catchJS,
-- * Threading
  forkAfter) where

import CPS

{- $unsafejs 
The 'unsafeJS' function is not a function per se: 
it is rather a macro, or a compilation directive. 
Its purpose is to provide a Haskell-accessible wrapper with 
proper type signature for an arbitrary Javascript 
code which obeys certain coding rules.

The function takes a string. Type of the return value 
does not matter: the function itself is never executed. Its applications 
are detected by the Javascript backend at the time of Javascript generation.

The function should be called with a string literal. Neither explicitly 
coded (with (:)) list of characters nor concatenation of two or more 
strings will work. The converter will report an error in this situation.

A valid example of using unsafeJS is shown below:

> global_YHC'_Primitive''primFloatPow a b = unsafeJS
>  "return Math.pow(exprEval(a), exprEval(b));"

This is a Javascript overlay (in the sense that it overlays the default 
Prelude definition of the (**) function) of a function that exponentiates
a 'Float' value.

Below is the Javascript representation of this function as generated
by the backend:

> strIdx["F_p6"] = "YHC.Primitive;primFloatPow";
> ...
> var F_p6=new HSFun("F_p6", 2, function(a, b){
> return Math.pow(exprEval(a), exprEval(b));});

Here are the rules that govern the usage of 'unsafeJS':

    * The 'unsafeJS' function is contained in the "UnsafeJS" module 
      and should be imported from there

    * Its argument must be a string literal, and nothing else

    * Its argument should be written entirely on a single line

    * Formal parameter names visible to Javascript are /a/, /b/, /c/, etc. 
      that is single lowercase letters

    * Number of formal parameters should match the Haskell type signature

    * It is recommended to name the function's formal parameters in Haskell 
      declaration in the same way they are visible to Javascript, i. e. /a/, /b/, /c/, etc.

    * Haskell values are passed to Javascript functions unevaluated: use exprEval to evaluate

    * Javascript code passed to 'unsafeJS' should not contain outermost 
      Javascript function declaration and curly braces: ycr2js will provide those

    * Javascript code is not limited in what it may contain; common sense must be 
      observed not to code in unsafe way when not really necessary: for instance 
      it is possible to change fields of a Haskell data object from Javascript, 
      but it is strongly discouraged: create a modified copy of the object 
      and leave the original unchanged, like a Haskell program would do.

    * /Javascript code must return a value/



-}

#ifndef __HADDOCK__

foreign import primitive unsafeJS :: String -> a

#else

unsafeJS :: String -> a

#endif

{- $unsafeprop 
Unsafe (type-wise) functions to get\/set\/check arbitrary properties
of Javascript objects. These functions are intended to implement typesafe 
getters\/setters of DOM element properties using IDL specificatons.
-}

-- |Retrieve the value of a named property on the given Javascript object
unsafeGetProperty :: String -> b -> CPS d c

unsafeGetProperty a b = toCPE (unsafeGetProperty' a b)
unsafeGetProperty' a b = unsafeJS "return exprEval(b)[exprEval(a)];"

-- |Check if a named property exists on the given Javascript object.
unsafeCheckProperty :: String -> b -> CPS d Bool

unsafeCheckProperty a b = toCPE (unsafeCheckProperty' a b)
unsafeCheckProperty' a b = unsafeJS "return !!exprEval(b)[exprEval(a)];"

-- |Modify in-place the value of a named property on the given Javascript object.
unsafeSetProperty :: String -> b -> c -> CPS d c

unsafeSetProperty a b c = toCPE (unsafeSetProperty' a b c)
unsafeSetProperty' a b c = unsafeJS "exprEval(c)[exprEval(a)]=exprEval(b);return c;"

-- |Check if the given value is null. This is not a CPS function. It does not
-- attempt to evaluate its argument.

unsafeIsNull :: a -> Bool

unsafeIsNull a = unsafeJS "return (!a);"

-- |Check if the given value is a String. This is not a CPS function. It does not
-- attempt to evaluate its argument. It may be useful to check if a value contains
-- an unevaluated string literal as attempt to check its constructor name will cause
-- evaluation to WHNF and therefore conversion into HSCons or HSEOL.

unsafeIsString :: a -> Bool

unsafeIsString a = unsafeJS "return (typeof a === 'string');"

-- |Convert an arbitrary value into a numeric value provided that
-- from the Javascript standpoint such value has numeric representation.
-- Otherwise results are not specified.
unsafeToNum :: (Num b) => a -> CPS c b

unsafeToNum a = toCPE (unsafeToNum' a)
unsafeToNum' a = unsafeJS "return new Number(exprEval(a));"

-- |Convert an arbitrary value into a 'String' value provided that
-- from the Javascript standpoint such value has string representation.
-- Otherwise results are not specified.
unsafeToString :: a -> CPS c String

unsafeToString a = toCPE (unsafeToString' a)
unsafeToString' a = unsafeJS "return new String(exprEval(a));"

{-| 
This function is similar to 'id' except that the type of the result
may differ from that of the argument. This function is useful when
it is necessary to cast a DOM value from its type to some other type
while representing the same object. An example: casting any value
that inherits from a Node:

> elementNode :: (CNode a) => a -> CPS b TNode
> elementNode = unsafeToSelf

-}
unsafeToSelf :: a -> CPS b c

unsafeToSelf a = toCPS (unsafeToSelf' a)
unsafeToSelf' a = unsafeJS "return a;"

-- |The most general function to catch Javascript runtime exceptions:
-- if no exception occurs during evaluation of an expression given as
-- the first argument, return the result of evaluation. Otherwise,
-- use a function specified as the second argument which takes the
-- exception object and returns a value of the same type as the failed
-- computation would have returned.

catchJS :: a          -- ^expression to be evaluated
        -> (b -> a)   -- ^exception handler (/b/ is a type of thrown exception object)
        -> a          -- ^value returned from either the expression or the handler

catchJS a b = unsafeJS
  "try {return exprEval(a);} catch(____e) {return exprEval(b)._ap([____e]);}"

-- |Fork execution after certain amount of time (the first argument, including 0) 
-- by setting timeout and passing a value to be evaluated then (the second argument) 
-- and right now (the third argument).

forkAfter :: Int -> b -> c -> c

forkAfter a b c = (fork' a b) `seq` c

fork' a b = unsafeJS
  "var t=contNum++;var s='exprEval(delCont['+t+']);delete delCont['+t+'];'; delCont[t]=b;window.setTimeout(s,exprEval(a)); return 0;"



