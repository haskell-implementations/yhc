{-|
    Equal checks if two CoreExpr's are equal ignoring any children
    expressions. Usually 'Eq' is what is wanted, but for some stuff
    this is more appropriate.
-}
module Yhc.Core.Equal(
    eqCoreExpr1,
    coreExpr1, CoreExpr1
    ) where

import Yhc.Core.Type
import Data.List


{-|
    Should be equivalent to:

    > eqCoreExpr1 x y = length xs == length ys && _x vs == _y vs
    >     where
    >         vs = replicate (length xs) (CoreVar "")
    >         (xs,_x) = uniplate x
    >         (ys,_y) = uniplate y
-}

eqCoreExpr1 = (?)

CoreCon a ? CoreCon b = a == b
CoreVar a ? CoreVar b = a == b
CoreFun a ? CoreFun b = a == b
CoreApp _ a ? CoreApp _ b = length a == length b
CoreLam a _ ? CoreLam b _ = a == b
CoreCase _ a ? CoreCase _ b = map fst a == map fst b
CoreLet a _ ? CoreLet b _ = map fst a == map fst b
CorePos a _ ? CorePos b _ = a == b
CoreLit a ? CoreLit b = a == b
_ ? _ = False



data CoreExpr1 = CoreCon1 CoreCtorName
              | CoreVar1 CoreVarName
              | CoreFun1 CoreFuncName
              | CoreApp1 Int
              | CoreLam1 [CoreVarName]
              | CoreCase1 [CorePat]
              | CoreLet1 [CoreVarName]
              | CorePos1 String
              | CoreLit1 CoreLit
                deriving (Ord,Eq,Show)


coreExpr1 :: CoreExpr -> CoreExpr1
coreExpr1 (CoreCon x) = CoreCon1 x
coreExpr1 (CoreVar x) = CoreVar1 x
coreExpr1 (CoreFun x) = CoreFun1 x
coreExpr1 (CoreApp x y) = CoreApp1 (length y)
coreExpr1 (CoreLam x y) = CoreLam1 x
coreExpr1 (CoreCase x y) = CoreCase1 (map fst y)
coreExpr1 (CoreLet x y) = CoreLet1 (map fst x)
coreExpr1 (CorePos x y) = CorePos1 x
coreExpr1 (CoreLit x) = CoreLit1 x
