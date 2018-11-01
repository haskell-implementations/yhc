
{-|
    In: \x -> y x
    
    x is bound
    
    y is free
-}
module Yhc.Core.FreeVar3(
    collectAllVars, collectBoundVars, collectFreeVars, countFreeVar,
    uniplateBoundVars,
    replaceFreeVars, replaceFreeVarsUnique,
    freeVars, getVar, getVars, duplicateExpr, checkFreeVar,
    uniqueBoundVarsCore, uniqueBoundVarsFunc, uniqueBoundVars
    ) where

import Yhc.Core.Type
import Yhc.Core.Uniplate
import Yhc.Core.UniqueId
import Yhc.Core.Internal.General

import Data.Char
import Data.List
import Data.Maybe
import Control.Monad.State
import Control.Monad.Identity


-- * Collection and Classification

-- | Which variables are mentioned in an expression
collectAllVars :: CoreExpr -> [CoreVarName]
collectAllVars = ordNub . concatMap f . universeExpr
    where
        f (CoreVar x) = [x]
        f (CoreLet bind x) = map fst bind
        f (CoreLam bind x) = bind
        f (CoreCase on alts) = concatMap (patVariables . fst) alts
        f x = []


-- | Which variables are introduced at any point,
--   i.e. LHS of a case alternative, or by a let
collectBoundVars :: CoreExpr -> [CoreVarName]
collectBoundVars = ordNub . concatMap f . universeExpr
    where
        f (CoreCase on alts) = concatMap (patVariables . fst) alts
        f (CoreLet bind x) = map fst bind
        f (CoreLam bind x) = bind
        f x = []


-- | Which variables are in the used in an expression
--   before being defined. No variable will occur more than once
collectFreeVars :: CoreExpr -> [CoreVarName]
collectFreeVars = f
    where
        -- f must ensure uniqueness at each stage
        f (CoreVar x) = [x]
        f (CoreCase on alt) = ordNub $ f on ++ concatMap g alt
        f (CoreLet bind x) = ordNub (f x ++ concatMap (f . snd) bind) \\ map fst bind
        f (CoreLam bind x) = f x \\ bind
        f x = ordNub $ concatMap f (children x)

        g (lhs,rhs) = f rhs \\ patVariables lhs


-- | Count the number of uses of a free variable.
--   If a variable is used in different branches of a case, it is only
--   considered to be the maximum of these two branches.
countFreeVar :: CoreVarName -> CoreExpr -> Int
countFreeVar s (CoreVar x) = if x == s then 1 else 0

countFreeVar s (CoreCase on alts) = countFreeVar s on + maximum (map g alts)
    where
        g (lhs,rhs) | s `elem` patVariables lhs = 0
                    | otherwise = countFreeVar s rhs

countFreeVar s (CoreLet bind x) | s `elem` map fst bind = 0
countFreeVar s (CoreLam bind x) | s `elem` bind = 0
countFreeVar s x = sum $ map (countFreeVar s) (children x)


-- * Uniplate style FreeVar stuff
-- Should really be used throughout, but free variable stuff
-- takes forever to get right, and don't want to break it now

-- | Get the variables that are defined to one-level depth
--   and a function to replace them
uniplateBoundVars :: CoreExpr -> ([CoreVarName], [CoreVarName] -> CoreExpr)
uniplateBoundVars (CoreLet bind x) = (lhs, \lhs -> CoreLet (zip lhs rhs) x)
    where (lhs,rhs) = unzip bind
uniplateBoundVars (CoreLam bind x) = (bind, \bind -> CoreLam bind x)
uniplateBoundVars (CoreCase on alts) = (children, \rep -> CoreCase on $ f rep alts)
    where
        children = concatMap (patVariables . fst) alts

        f rep ((PatCon x xs, y):alts) = (PatCon x r, y) : f rs alts
            where (r,rs) = splitAt (length xs) rep
        f rep (x:xs) = x : f rep xs
        f [] [] = []

uniplateBoundVars x = ([], const x)


-- * Operations


-- | Replace all free occurances of variables with a new expression
replaceFreeVars :: [(CoreVarName, CoreExpr)] -> CoreExpr -> CoreExpr
replaceFreeVars ren = runIdentity . replaceFreeVarsWith return ren


replaceFreeVarsUnique :: UniqueIdM m => [(CoreVarName, CoreExpr)] -> CoreExpr -> m CoreExpr
replaceFreeVarsUnique ren = replaceFreeVarsWith duplicateExpr ren



replaceFreeVarsWith :: Monad m => (CoreExpr -> m CoreExpr) -> [(CoreVarName, CoreExpr)] -> CoreExpr -> m CoreExpr
replaceFreeVarsWith dupe ren x =
    case x of
        CoreVar x -> maybe (return $ CoreVar x) dupe (lookup x ren)
        
        CoreLet bind x -> descendM (replaceFreeVarsWith dupe ren2) (CoreLet bind x)
            where ren2 = remove (map fst bind)

        CoreLam bind x -> liftM (CoreLam bind) $ replaceFreeVarsWith dupe (remove bind) x

        CoreCase on alts -> do
                on <- replaceFreeVarsWith dupe ren on
                alts <- mapM f alts
                return $ CoreCase on alts
            where
                f (lhs,rhs) = liftM ((,) lhs) $ replaceFreeVarsWith dupe (remove (patVariables lhs)) rhs
        
        x -> descendM (replaceFreeVarsWith dupe ren) x
    where
        remove xs = filter ((`notElem` xs) . fst) ren



-- | Check that the free variables in the second expression
--   are also in the first one. It usually indicates an error to
--   introduce new free variables in transformation.
--
--   Return True for safe, False for probably buggy.
checkFreeVar :: CoreExpr -> CoreExpr -> Bool
checkFreeVar orig new = null $ collectFreeVars new \\ collectFreeVars orig


-- * Unique Monad Support


freeVars :: Char -> [String]
freeVars c = [c:show i | i <- [1..]]


getVar :: UniqueIdM m => m CoreVarName
getVar = liftM (('v':) . show) nextId


getVars :: UniqueIdM m => Int -> m [CoreVarName]
getVars n = replicateM n getVar


duplicateExpr :: UniqueIdM m => CoreExpr -> m CoreExpr
duplicateExpr = uniqueBoundVarsExpr

-- | Replace all variables which are locally defined with new names
--   from the monad.
uniqueBoundVarsExpr :: UniqueIdM m => CoreExpr -> m CoreExpr
uniqueBoundVarsExpr = uniqueBoundVarsExprWith []


-- | Local version,  which allows a substitution set to be passed through
uniqueBoundVarsExprWith :: UniqueIdM m => [(String,String)] -> CoreExpr -> m CoreExpr
uniqueBoundVarsExprWith ren x = let f = uniqueBoundVarsExprWith in
    case x of
        CoreVar x -> return $ CoreVar $ fromMaybe x (lookup x ren)

        CoreCase on alts -> do
                on2 <- f ren on
                alts2 <- mapM g alts
                return $ CoreCase on2 alts2
            where
                g (PatCon c vars, rhs) = do
                    vars2 <- getVars (length vars)
                    let ren2 = zip vars vars2 ++ ren
                    rhs2 <- f ren2 rhs
                    return (PatCon c vars2, rhs2)
                g (lhs,rhs) = do
                    rhs2 <- f ren rhs
                    return (lhs,rhs2)

        CoreLet bind x -> do
                let (lhs,rhs) = unzip bind
                lhs2 <- getVars (length lhs)
                let ren2 = zip lhs lhs2 ++ ren
                rhs2 <- mapM (f ren2) rhs
                x2 <- f ren2 x
                return $ CoreLet (zip lhs2 rhs2) x2

        CoreLam bind x -> do
                bind2 <- getVars (length bind)
                let ren2 = zip bind bind2 ++ ren
                x2 <- f ren2 x
                return $ CoreLam bind2 x2

        _ -> descendExprM (f ren) x


-- | Take care: If v123 is a free variable, then make sure getVar starts above that
uniqueBoundVars :: UniqueIdM m => CoreExpr -> m CoreExpr
uniqueBoundVars x = do
        let seen = [read i | 'v':i <- collectFreeVars x, all isDigit i, not $ null i]
            limit = maximum (0:seen) + 1
        i <- getIdM
        putIdM (max i limit)
        uniqueBoundVarsExpr x


-- | Make a whole function have unique free variables
uniqueBoundVarsFunc :: UniqueIdM m => CoreFunc -> m CoreFunc
uniqueBoundVarsFunc x | isCorePrim x = return x
uniqueBoundVarsFunc (CoreFunc name args body) = do
    args2 <- getVars (length args)
    body2 <- uniqueBoundVarsExprWith (zip args args2) body
    return $ CoreFunc name args2 body2


-- | Make a whole Core program have unique free variables.
uniqueBoundVarsCore :: UniqueIdM m => Core -> m Core
uniqueBoundVarsCore core = do
    funcs2 <- mapM uniqueBoundVarsFunc $ coreFuncs core
    return $ core{coreFuncs = funcs2}
