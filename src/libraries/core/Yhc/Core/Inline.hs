
{- |
    Inlining module.
    
    This module will let you perform some inlining on Yhc.Core code. The 'InlineMode' argument
    lets you select what algorithm is used. All should be terminating, and none should
    increase the number of function calls in a program.
    
    For comparison, GHC's inlining mode is more powerful than 'InlineForward', but less
    powerful than 'InlineFull'. (And just so people understand, powerful does not mean more
    performance, it means more inlining - the two are not always the same!)
    
    'InlineNone'
    
    No inlining. Equivalent to 'id' :)
    
    'InlineAlias'
    
    A function is inlined if it is aliased to another function.
    
    A function is aliased if all it does is call another function with the
    same arguments in the same order. i.e.
    
    > f x y z = g x y z
    
    Note that a function is not aliased if any argument is duplicated, the
    RHS is a primitive or a constructor, or the arguments are reordered.
    
    This restriction means that inlining can even occur when f is used
    higher order, g can be replaced.
    
    This mode will never increase the code size.
    
    'InlineForward'
    
    A function is inlined if it is a forwarder.
    
    A function is a forwarder if all it does is call another function,
    using only the given arguments, possibly reordered but not duplicated.
    A forwarder can also be a single constant value, or a simple argument
    value (a projection), or a constructor with no arguments. i.e.
    
    > f x y z = 12
    > f x y z = g z y
    > f x y z = x
    
    The function is only inlined if it is called saturated.
    
    This mode will never increase the code size.
    
    'InlineCallOnce'
    
    A function is inlined if it is a forwarder, or if there is only one
    caller. Only inlined if called saturated. Will never increase the code
    size.
    
    'InlineFull'
    
    This does the most inlining it can, but never inlines the same function
    more than once in a given expression - to ensure termination. Also doesn't
    inline CAF's, since that would go wrong. Large functions, recursive functions,
    duplicated arguments etc - all are inlined without question.
    
    Duplicated arguments are moved into a let, to ensure they are not computed
    additional times.
    
    This mode is more than likely to increase the code size in most programs.
-}

module Yhc.Core.Inline(
    coreInline, InlineMode(..),
    coreInlineFunc, coreInlineFuncLambda
    ) where

import Yhc.Core.Type
import Yhc.Core.Uniplate
import Yhc.Core.FreeVar

import qualified Data.Map as Map
import Data.Maybe
import Data.List


data InlineMode = InlineNone -- ^ no inlining at all
                | InlineAlias -- ^ f a b c = g a b c, calls to g become calls to f
                | InlineForward -- ^ f a b c = g a b, g b a, a (g may be a constructor)
                | InlineCallOnce -- ^ f is called only once
                | InlineFull -- ^ If you can inline it, do so! Breaks on first recursive call


coreInline :: InlineMode -> Core -> Core
coreInline InlineNone core = core
coreInline InlineAlias core = coreInlineAlias core
coreInline InlineForward core = inlineNormal (analyseForward core) core
coreInline InlineCallOnce core = inlineNormal (analyseForward core `Map.union` analyseCallOnce core) core
coreInline InlineFull core = inlineNormal (analyseFull core) $ coreInlineAlias core


coreInlineAlias core = inlineAlias (analyseAlias core) core

---------------------------------------------------------------------
-- INLINING OPERATIONS


inlineAlias :: Map.Map CoreFuncName CoreFuncName -> Core -> Core
inlineAlias rep core = transformExpr f core
    where
        f (CoreFun x) = CoreFun $ Map.findWithDefault x x rep
        f x = x


inlineNormal :: Map.Map CoreFuncName CoreFunc -> Core -> Core
inlineNormal rep core = applyFuncCore f core
    where
        f (CoreFunc name args body) = CoreFunc name args $ transformExpr (g [name]) body
        f x = x

        g done x = fromMaybe x $ do
            (CoreFun fn,args) <- return $ fromCoreApp x
            func <- Map.lookup fn rep
            True <- return $ fn `notElem` done
            res <- coreInlineFunc func args
            return $ transformExpr (g (fn:done)) res



---------------------------------------------------------------------
-- INLINING ANALYSIS


analyseAlias :: Core -> Map.Map CoreFuncName CoreFuncName
analyseAlias core = transForward
    where
        -- where there is a single forwarder
        basicForward :: Map.Map String String
        basicForward = Map.fromList $ concatMap f (coreFuncs core)
            where
                f (CoreFunc name args (CoreApp (CoreFun x) xs)) | map CoreVar args == xs = [(name,x)]
                f _ = []

        -- what is the transitive closure of the basicForward
        transForward :: Map.Map String String
        transForward = Map.mapWithKey (\k v -> f [k] v) basicForward
            where
                f done name =
                    case Map.lookup name basicForward of
                        Just x | name `notElem` done -> f (name:done) x
                        _ -> name


analyseForward :: Core -> Map.Map CoreFuncName CoreFunc
analyseForward core = Map.fromList
        [(name, func) | func@(CoreFunc name _ bod) <- coreFuncs core, canInline bod]
    where
        canInline (CorePos _ x) = canInline x
        canInline (CoreApp x xs) = isGoodFun x && all isGoodArg xs && disjoint [i | CoreVar i <- xs]
        canInline x = isCoreCon x || isCoreFun x || isGoodArg x
        
        isGoodFun x = isCoreFun x || isCoreCon x
        
        isGoodArg x = isCoreVar x || isSmallConst x
        
        isSmallConst x = isCoreLit x && not (isCoreStr $ fromCoreLit x)


analyseCallOnce :: Core -> Map.Map CoreFuncName CoreFunc
analyseCallOnce core = Map.fromList
        [(name,func) | func@(CoreFunc name (_:_) _) <- coreFuncs core, Just True == Map.lookup name once]
    where
        once :: Map.Map CoreFuncName Bool -- True is once, False is many
        once = foldl f Map.empty [x | CoreFun x <- universeExpr core]
        f mp x = Map.insertWith (\_ _ -> False) x True mp


analyseFull :: Core -> Map.Map CoreFuncName CoreFunc
analyseFull core = Map.fromList [(name,func) | func@(CoreFunc name (_:_) _) <- coreFuncs core]


disjoint x = length (nub x) == length x


---------------------------------------------------------------------
-- INLINING ACTIONS

-- | Inline a function, fails if it would produce a lambda
--   See 'coreInlineFuncLambda' for a version without this property
coreInlineFunc :: CoreFunc -> [CoreExpr] -> Maybe CoreExpr
coreInlineFunc func@(CoreFunc name params2 body2) args
     | nparams > nargs = Nothing
     | otherwise = Just res
     where
        res = coreApp subst (drop nparams args)
     
        (nargs, nparams) = (length args, length params2)
        argvars = concatMap collectAllVars args
        allvars = ['v':show i | i <- [1..]] \\ (params2 ++ argvars ++ collectAllVars body2)
        
        (params,rest) = splitAt nparams allvars
        body = uniqueBoundVarsWith rest $ replaceFreeVars (zip params2 (map CoreVar params)) body2
        newvars = rest \\ collectAllVars body

        (dupe,once) = partition (\(lhs,rhs) -> requiresLet rhs && countFreeVar lhs body > 1) (zip params args)
        requiresLet x = not (isCoreVar x || isCoreFun x)
        dupnew = zip newvars dupe
        
        binds = [(new,a) | (new,(p,a)) <- dupnew]
        reps = [(p,CoreVar new) | (new,(p,a)) <- dupnew] ++ once
        
        subst = coreLet binds (replaceFreeVars reps body)


-- | Inline a function, generating a lambda if necessary
--   NOTE: Should this return a CoreLam now we have this in the AST
coreInlineFuncLambda :: CoreFunc -> [CoreExpr] -> ([String], CoreExpr)
coreInlineFuncLambda func@(CoreFunc name params body) args =
        (extraArgs, fromJust $ coreInlineFunc func (args ++ map CoreVar extraArgs))
    where
        extraArgs = drop (length args) (coreFuncArgs func)
