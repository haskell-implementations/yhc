
module Yhc.Core.RecursiveLet(
    isCoreLetRec,
    removeRecursiveLet,
    reduceRecursiveLet
    ) where

import Yhc.Core.Type
import Yhc.Core.Uniplate
import Yhc.Core.FreeVar
import Yhc.Core.UniqueName

import Control.Monad
import Control.Monad.State
import Data.List


-- | Remove recursive lets
--
--   Let's are rearranged so a variable is not used in the defining block
removeRecursiveLet :: Core -> Core
removeRecursiveLet = uniqueFuncsSplit (remRecLet True)


-- | Reduce the number of recursive lets, but splitting lets
--   which have recursive bindings, but can be linearised
reduceRecursiveLet :: Core -> Core
reduceRecursiveLet = uniqueFuncsSplit (remRecLet False)


remRecLet :: Monad m => Bool -> m CoreFuncName -> (CoreFunc -> m ()) -> CoreExpr -> m CoreExpr
remRecLet always newFunc addFunc = f
    where
        f (CoreLet [] x) = f x
        
        -- handle the variables which are mixed up, but not actually recursive
        -- let a = b; b = 1 in a
        f (CoreLet binds x) | not (null free) = do
                free2 <- mapM (\(a,b) -> liftM ((,) a) $ f b) free
                locked2 <- f (CoreLet locked x)
                return $ CoreLet free2 locked2
            where
                defined = map fst binds
                (locked,free) = partition (isLocked . snd) binds

                isLocked = any (`elem` defined) . collectFreeVars
        
        -- handle the truely recursive ones
        -- let xs = x:xs in xs
        f (CoreLet binds x) | always = do
                names <- replicateM (length binds) newFunc
                let binds2 = zip lhs (map (\x -> CoreApp (CoreFun x) (map CoreVar vars)) names)
                newfuncs <- zipWithM (g (zip lhs names) binds2) names rhs
                mapM_ addFunc newfuncs
                
                x2 <- f x
                return $ CoreLet binds2 x2
            where
                (lhs,rhs) = unzip binds
                vars = nub (concatMap collectFreeVars rhs) \\ lhs
                
                g mapping binds2 name rhs = do
                        let free = collectFreeVars rhs
                            binds3 = filter ((`elem` free) . fst) binds2
                        body <- f $ CoreLet binds3 rhs
                        return $ CoreFunc name vars body

        f x = descendM f x


-- | Is a CoreLet recursive, i.e. do any of the introduced variables (LHS of bind)
--   also show up in the RHS of bind.
--
--   Returns False if the expression is not a CoreLet.
isCoreLetRec :: CoreExpr -> Bool
isCoreLetRec (CoreLet bind xs) = not $ null $ map fst bind `intersect` concatMap (collectFreeVars . snd) bind
isCoreLetRec x = False
