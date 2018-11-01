
{-|
    In: \x -> y x
    
    x is bound
    
    y is free
-}
module Yhc.Core.FreeVar(
    collectAllVars, collectBoundVars, collectFreeVars,
    countFreeVar, replaceFreeVars,
    variableSupply,
    uniqueBoundVars, uniqueBoundVarsWith, uniqueBoundVarsWithout,
    uniqueBoundVarsCore, uniqueBoundVarsFunc
    ) where

import Yhc.Core.FreeVar3(collectAllVars, collectBoundVars, collectFreeVars, countFreeVar, replaceFreeVars)
import Yhc.Core.Type
import Yhc.Core.Play
import Yhc.Core.Internal.General

import Data.List
import Data.Maybe


-- sorted nub
snub :: Ord a => [a] -> [a]
snub = map head . group . sort


-- | Given a prefix, generate a stream of variables
--   Each will be unique in the series
variableSupply :: Char -> [String]
variableSupply c = [c:show i | i <- [1..]]


-- | Just 'uniqueFreeVarsWith', but with a default set of variables
uniqueBoundVars :: CoreExpr -> CoreExpr
uniqueBoundVars = uniqueBoundVarsWith (variableSupply 'v')


-- | Just 'uniqueFreeVarsWith', but with a certain set excluded
uniqueBoundVarsWithout :: [String] -> CoreExpr -> CoreExpr
uniqueBoundVarsWithout xs = uniqueBoundVarsWith (variableSupply 'v' \\ xs)


-- | Replace all variables which are locally defined with new names
--   from the given list. Raises an error if not enough free variables
--   are supplied
--
--   If any in the new list clashes with a name in 'collectFreeVars' this
--   will return a program with different semantics!
--
--   Property: collectFreeVars (uniqueFreeVarsWith newvars x) `subset` newvars
--
uniqueBoundVarsWith :: [String] -> CoreExpr -> CoreExpr
uniqueBoundVarsWith new = snd . f [] new
    where
        f :: [(String,String)] -> [String] -> CoreExpr -> ([String], CoreExpr)
        f ren new x = 
            case x of
                CoreVar x -> (new, CoreVar $ fromMaybe x (lookup x ren))
                
                CoreCase on alts -> (new3, CoreCase on2 alts2)
                    where
                        (new2,on2) = f ren new on
                        (new3,alts2) = mapAccumL g new alts
                        
                        g new (lhs_,rhs) = (new3,(exprToPat lhs2,rhs2))
                            where
                                lhs = patToExpr lhs_
                            
                                vars = [x | CoreVar x <- allCore lhs]
                                (vars2,new2) = splitAt (length vars) new
                                ren2 = zip vars vars2 ++ ren
                                
                                (_,lhs2) = f ren2 [] lhs
                                (new3,rhs2) = f ren2 new2 rhs
                
                CoreLet bind x -> (new4, CoreLet (zip lhs2 rhs2) x2)
                    where
                        (lhs,rhs) = unzip bind
                        (lhs2,new2) = splitAt (length bind) new
                        ren2 = zip lhs lhs2 ++ ren
                        
                        (new3,rhs2) = mapAccumL (f ren2) new2 rhs
                        (new4,x2) = f ren2 new3 x
                
                CoreLam bind x -> (new3, CoreLam bind2 x2)
                    where
                        (bind2,new2) = splitAt (length bind) new
                        (new3,x2) = f (zip bind bind2 ++ ren) new2 x
                
                _ -> (new2, setChildrenCore x child2)
                    where
                        (new2, child2) = mapAccumL (f ren) new (getChildrenCore x)


-- | Make a whole Core program have unique free variables.
--   Between functions, they may share variables
uniqueBoundVarsCore :: Core -> Core
uniqueBoundVarsCore = applyFuncCore uniqueBoundVarsFunc


-- | Make a whole function have unique free variables
uniqueBoundVarsFunc :: CoreFunc -> CoreFunc
uniqueBoundVarsFunc x@(CorePrim{}) = x
uniqueBoundVarsFunc (CoreFunc name args body)
        = CoreFunc name args2 (replaceFreeVars (zip args (map CoreVar args2)) (uniqueBoundVarsWith free body))
    where
        (args2,free) = splitAt (length args) (variableSupply 'v' \\ (args ++ collectAllVars body))
