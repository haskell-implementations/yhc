
module Yhc.Core.Saturated(coreSaturated) where

import Yhc.Core.Type
import qualified Data.Map as Map


-- | Given an expr (normally a 'CoreApp')
--   say if it is saturated or not.
coreSaturated :: Core -> (CoreExpr -> Bool)
coreSaturated core =
        \x -> case x of
            CoreApp (CoreFun x) ys -> f funcArity x ys
            CoreApp (CoreCon x) ys -> f ctorArity x ys
            _ -> False
    where
        ctorArity = Map.fromList [(name, length args) | dat <- coreDatas core, (CoreCtor name args) <- coreDataCtors dat]
        funcArity = Map.fromList [(name, length args) | CoreFunc name args _ <- coreFuncs core]

        f mp x ys = case Map.lookup x mp of
                        Nothing -> False
                        Just xn -> xn == length ys
