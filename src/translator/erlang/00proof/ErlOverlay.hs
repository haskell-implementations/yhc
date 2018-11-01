module ErlOverlay where

import YHC.Internal
import YHC.Primitive

foreign import erlang "hserl:force" force :: a -> a

global_Prelude''YHC'_Internal'_IO''Prelude'_Monad'''gt'gt'eq x y = 
  IO (ioForceBind x y) where
     ioForceBind (IO xf) y w = let xe = (force xf) w
                               in case xe of
                                 _E xv -> case y (force xv) of
                                   IO yf -> yf w



