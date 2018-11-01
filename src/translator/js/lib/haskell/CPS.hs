-----------------------------------------------------------------------------
-- |
-- Module      :  CPS
-- Copyright   :  (c) Dmitry Golubovsky, 2007       
-- License     :  BSD-style
-- 
-- Maintainer  :  Dmitry Golubovsky <golubovsky@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Continuation-Passing Style
--
-----------------------------------------------------------------------------

module CPS (
-- $cps
   CPS,
   toCPS,
   toCPE,
   cps2M
) where

{- $cps
Continuation-passing style (CPS) is used by the Javascript backend to implement
sequences of actions. Basic information (and links to more advanced topics on CPS) 
can be found at the following Haskell Wiki page:
<http://www.haskell.org/haskellwiki/Continuation>.
-}

-- |The CPS c a represents a computation which is a part of a larger computation
-- with final value of type /c/ and intermediate result of type /a/.

type CPS c a = (a -> c) -> c

-- |Convert a function to CPS style.

toCPS :: a -> CPS b a

toCPS x = \k -> k x

-- |Convert a function to CPS style and force evaluation of its return value.
-- Do not use on nullary functions unless CAF is desired. For instance,
-- the nullary function 'Debug.Profiling.getTimeStamp' has to be coded with explicit
-- continuation, like this: @getTimeStamp c = c $! (getTimeStamp\' 0)@
-- instead of @toCPE (getTimeStamp\' 0)@.

toCPE :: a -> CPS b a

toCPE x = \k -> x `seq` (k x) 

-- |This function may be used to bring a CPS computation into a monadic code.
-- Note that final rather than intermediate value will be returned.

cps2M :: (Monad m) => CPS a a -> m a

cps2M x = return $! x id


