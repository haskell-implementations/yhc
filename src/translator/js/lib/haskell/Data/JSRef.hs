-----------------------------------------------------------------------------
-- |
-- Module      :  Data.JSRef
-- Copyright   :  (c) Dmitry Golubovsky, 2007       
-- License     :  BSD-style
-- 
-- Maintainer  :  Dmitry Golubovsky <golubovsky@gmail.com>
-- Stability   :  stable
-- Portability :  non-portable (needs Yhc Javascript backend)
--
-- Mutable Javascript Objects
--
-------------------------------------------------------------------------------


module Data.JSRef (
-- $jsref
    JSRef
  , newJSRef
  , newStrictJSRef
  , readJSRef
  , writeJSRef
  , modifyJSRef
  ) where

import UnsafeJS
import CPS

{- $jsref
To implement low-level operations, it is necessary to have the interface
to mutable Javascript objects, similar to IORefs. JSRefs provide such an interface.
Just like IORefs, JSRefs may be created, read, written, and modified with a function.
An example of using JSRefs is module "Control.Concurrent.JSThreads" which implements
message boxes (mutable in-place) with JSRefs.

From Javacript side, JSRefs are just plain Javascript oblects with property named
/_val/ which holds the stored value.

Operations with JSRefs that store data, are strict on values to be stored unless
specified otherwise.
-}

-- |An opaque data type parameterized by the type of the stored value.

data JSRef a

-- |Create a mutable Javascript object and initialize it (strict version).

newStrictJSRef :: a -> CPS b (JSRef a)

newStrictJSRef a = toCPE (newStrictJSRef' a)
newStrictJSRef' a = unsafeJS "return {_val:exprEval(a)};"


-- |Create a mutable Javascript object and initialize it (non-strict version).

newJSRef :: a -> CPS b (JSRef a)

newJSRef a = toCPE (newJSRef' a)
newJSRef' a = unsafeJS "return {_val:a};"

-- |Read the stored value from a mutable Javascript object.

readJSRef :: JSRef a -> CPS b a

readJSRef r = unsafeGetProperty "_val" r

-- |Store a value in a mutable Javascript object. Expression representing a value
-- will be evaluated, and the result of evaluation stored.

writeJSRef :: JSRef a -> a -> CPS b ()

writeJSRef r v k = unsafeSetProperty "_val" v r $ \x -> x `seq` k ()

-- |Apply a function to the stored value, and store the result.

modifyJSRef :: JSRef a  -- ^mutable object to modify
           ->  (a -> a) -- ^modifying function
           ->  CPS b () -- ^return value

modifyJSRef a b k = readJSRef a $ \v ->
                    writeJSRef a (b v) $ \x -> x `seq` k ()

