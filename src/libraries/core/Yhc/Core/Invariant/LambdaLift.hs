
module Yhc.Core.Invariant.LambdaLift(coreLambdaLift) where

import Yhc.Core.Type
import Yhc.Core.Uniplate
import Yhc.Core.UniqueName
import Yhc.Core.FreeVar
import Data.List


coreLambdaLift :: Core -> Core
coreLambdaLift = coreLambdaName . coreLambdaClosure


coreLambdaName :: Core -> Core
coreLambdaName = uniqueFuncsSplit f
    where
        f newFunc addFunc = transformM (g newFunc addFunc)

        g newFunc addFunc (CoreLam bind body) = do
            newname <- newFunc
            addFunc $ CoreFunc newname bind body
            return $ CoreFun newname
        g newFunc addFunc x = return x


coreLambdaClosure :: Core -> Core
coreLambdaClosure = transformExpr f
    where
        f x@(CoreLam bind body) = coreApp (CoreLam (free++bind) body) (map CoreVar free)
            where free = nub $ collectFreeVars x
        f x = x
