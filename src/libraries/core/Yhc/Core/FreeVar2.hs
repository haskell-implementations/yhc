
{-|
    In: \x -> y x
    
    x is bound
    
    y is free
-}
module Yhc.Core.FreeVar2(
    FreeVar, runFreeVars, freeVars,
    putVars, getVars, getVar, deleteVars,
    collectAllVars, collectBoundVars, collectFreeVars,
    countFreeVar, replaceFreeVars,
    uniqueBoundVarsCore, uniqueBoundVarsFunc, uniqueBoundVars
    ) where


import Yhc.Core.FreeVar3(collectAllVars, collectBoundVars, collectFreeVars, countFreeVar, replaceFreeVars)
import Control.Monad.State

import Yhc.Core.Type
import Yhc.Core.Uniplate
import Yhc.Core.Internal.General

import Data.List
import Data.Maybe



newtype FreeVar a = FreeVar {fromFreeVar :: State [String] a}

instance Monad FreeVar where
    return a = FreeVar (return a)
    (FreeVar x) >>= f = FreeVar (x >>= fromFreeVar . f)


putVars :: [String] -> FreeVar ()
putVars xs = FreeVar (put xs)


getVars :: FreeVar [String]
getVars = FreeVar get

getVar :: FreeVar String
getVar = do (x:xs) <- getVars
            putVars xs
            return x


deleteVars :: [String] -> FreeVar ()
deleteVars xs = FreeVar (modify (\\ xs))


runFreeVars :: FreeVar a -> a
runFreeVars (FreeVar x) = evalState x (freeVars 'v')


freeVars :: Char -> [String]
freeVars c = [c:show i | i <- [1..]]



-- | Replace all variables which are locally defined with new names
--   from the given list. Raises an error if not enough free variables
--   are supplied
--
--   If any in the new list clashes with a name in 'collectFreeVars' this
--   will return a program with different semantics!
--
--   Property: collectFreeVars (uniqueFreeVarsWith newvars x) `subset` newvars
--
uniqueBoundVars :: CoreExpr -> FreeVar CoreExpr
uniqueBoundVars = f []
    where
        f :: [(String,String)] -> CoreExpr -> FreeVar CoreExpr
        f ren x = 
            case x of
                CoreVar x -> return $ CoreVar $ fromMaybe x (lookup x ren)
                
                CoreCase on alts -> do
                        on2 <- f ren on
                        alts2 <- mapM g alts
                        return $ CoreCase on2 alts2
                    where
                        g (lhs,rhs) = do
                            lhs <- return $ patToExpr lhs
                            let vars = [x | CoreVar x <- universeExpr lhs]
                            vars2 <- getVarsN (length vars)
                            let ren2 = zip vars vars2 ++ ren

                            lhs2 <- f ren2 lhs
                            rhs2 <- f ren2 rhs
                            return (exprToPat lhs2, rhs2)

                CoreLet bind x -> do
                        let (lhs,rhs) = unzip bind
                        lhs2 <- getVarsN (length lhs)
                        let ren2 = zip lhs lhs2 ++ ren
                        
                        rhs2 <- mapM (f ren2) rhs
                        x2 <- f ren2 x
                        return $ CoreLet (zip lhs2 rhs2) x2

                CoreLam bind x -> do
                        bind2 <- getVarsN (length bind)
                        let ren2 = zip bind bind2 ++ ren
                        x2 <- f ren2 x
                        return $ CoreLam bind2 x2

                _ -> descendExprM (f ren) x


        getVarsN :: Int -> FreeVar [String]
        getVarsN n = do
            ys <- getVars
            let (used,keep) = splitAt n ys
            putVars keep
            return used


-- | Make a whole Core program have unique free variables.
uniqueBoundVarsCore :: Core -> FreeVar Core
uniqueBoundVarsCore core = do
    funcs2 <- mapM uniqueBoundVarsFunc $ coreFuncs core
    return $ core{coreFuncs = funcs2}


-- | Make a whole function have unique free variables
uniqueBoundVarsFunc :: CoreFunc -> FreeVar CoreFunc
uniqueBoundVarsFunc x@(CorePrim{}) = return x
uniqueBoundVarsFunc (CoreFunc name args body) = do
        vars <- getVars
        let (args2,rest) = splitAt (length args) vars
        putVars rest
        body2 <- uniqueBoundVars (replaceFreeVars (zip args (map CoreVar args2)) body)
        return $ CoreFunc name args2 body2
