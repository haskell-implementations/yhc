----------------------------------------------------------------------------
-- |
-- Module      :  Debug.Profiling
-- Copyright   :  (c) Dmitry Golubovsky, 2007       
-- License     :  BSD-style
-- 
-- Maintainer  :  Dmitry Golubovsky <golubovsky@gmail.com>
-- Stability   :  stable
-- Portability :  non-portable (requires Javascript backend)
--
-- Interface to profiling facilities 
--
----------------------------------------------------------------------------

module Debug.Profiling (
-- $prof
--
-- *Timestamp Functions
    getTimeStamp
  , getTimeDiff
-- *Profiling Functions
  , readProfile
  , clearProfile) where

import UnsafeJS
import CPS

{- $prof
Most functions of this module (except timestamp functions) require 
the Profiling.js script to be included NEXT TO Runtime.js on the page.
-}

{- |
Get the time difference in ms since given moment of time (using
this function with 0 is equivalent to 'getTimeStamp').
In order to time some computation, the following code may be used:

>  getTimeStamp $ \t1 ->
>  -- action to time
>  getTimeDiff t1 $ \dt -> ...

-}

getTimeDiff :: Int -> CPS b Int

getTimeDiff t = getTimeStamp $ \t0 -> toCPE (t0 - t)

-- |Get current time in ms since 1970 (for performance measurements)
-- This function obtains current time from the browser.

getTimeStamp :: CPS b Int

getTimeStamp c = c $! (getTimeStamp' 0)
getTimeStamp' a = unsafeJS "return new Date().getTime();"

-- |Get profiling data for a given category. Returns a list of tuples containing
-- profiling key and count for the key. If the boolean argument is True,
-- profiling key will be looked for in the string index. Unsuccessful lookups
-- result in key enclosed in curly brackets.

readProfile :: String -- ^profiling category (e. g. \"body_hs\")
            -> Bool   -- ^look up function names in the string index
            -> CPS b [(String, Int)] -- ^returned value: list of 2-tuples
                                     -- consisting of function name and
                                     -- number of times the function was called.

readProfile a b = toCPE (readProfile' a b)
readProfile' a b = unsafeJS "return readProfile (exprEval(a), exprEval(b));"

-- |Clear the profiling hashtable. All function call counters will be lost.

clearProfile :: CPS b Int

clearProfile c = c $! (clearProfile' 0)
clearProfile' a = unsafeJS "resetProfile(); return 0;"

