{-
THIS MODULE NEEDS REDRAFTING

With the new rewrite semantics of traverse it should be possible to
have a terminating, confluent, rewriting version - which allows others
to add to the rules.

Would also be nice if we could specify the free variable properties
more efficiently, and only once.
-}


module Yhc.Core.Simplify(
    coreSimplify, coreSimplifyExpr,
    coreSimplifyCaseCon, coreSimplifyCaseCase, coreSimplifyCaseLet,
    coreSimplifyExprUnique, coreSimplifyExprUniqueExt
    ) where

import Data.List
import Data.Maybe
import Control.Monad
import Yhc.Core.Internal.General

import Yhc.Core.Type
import Yhc.Core.Uniplate
import Yhc.Core.FreeVar3(duplicateExpr)
import Yhc.Core.FreeVar
import Yhc.Core.UniqueId



coreSimplify :: UniplateExpr a => a -> a
coreSimplify x = context $ map coreSimplifyExpr children
    where (children,context) = uniplateExpr x



-- | Simplify a single Core Expr.
--
--   Performs NO inlining, guaranteed to run in same or better
--   space and time. May increase code size.
--
--   Bugs lurk here, with inadvertant free variable capture. Move to
--   a proper free variable monad and a guarantee of uniqueness
coreSimplifyExpr :: CoreExpr -> CoreExpr
coreSimplifyExpr = transformExpr f
    where
        f (CoreCase (CoreFun x) alts) = f (CoreCase (CoreApp (CoreFun x) []) alts)
        
        f o@(CoreCase on alts) | isCoreCon $ fst $ fromCoreApp on = transformExpr f $ coreSimplifyCaseCon o
        f o@(CoreCase (CoreCase _ _) _) = transformExpr f $ coreSimplifyCaseCase o
        f o@(CoreCase (CoreLet _ _) _) = transformExpr f $ coreSimplifyCaseLet o

        f orig@(CoreApp (CoreCase _ _) _) = f $ CoreCase on (map g alts)
            where
                CoreApp (CoreCase on alts) args = uniqueExpr orig
                g (lhs,rhs) = (lhs, f $ CoreApp rhs args)
        
        f (CoreCase (CoreLet bind on) alts) = f $ CoreLet bind (f $ CoreCase on alts)
        
        f (CoreLet bind x) = coreLet many (transformExpr f $ replaceFreeVars once x)
            where
                bindVars = [i | CoreVar i <- concatMap (universeExpr . snd) bind]
                (once,many) = partition (uncurry isValid) bind
                
                isValid lhs rhs = lhs `notElem` bindVars && (isSimple rhs || countFreeVar lhs x <= 1)
                
                isSimple (CoreApp x []) = isSimple x
                isSimple (CoreFun x) = True
                isSimple (CorePos x y) = isSimple y
                isSimple (CoreVar x) = True
                isSimple (CoreApp (CorePos _ (CoreFun name)) args) = isSimple (CoreApp (CoreFun name) args)
                isSimple _ = False

        f (CoreLet binds (CoreCase on alts1))
            | disjoint (universeExprVar on) (map fst binds) = f $ CoreCase on (map g alts1)
            where g (lhs,rhs) = (lhs,f $ coreLet (filter ((`notElem` patVariables lhs) . fst) binds) $ f rhs)
        
        f (CoreApp (CoreApp x xs) ys) = f $ CoreApp x (xs++ys)
        
        f o@(CoreApp (CoreLam bind x) args) = transformExpr f $
                coreApp (coreLam bindnew (replaceFreeVars rep x)) args2
            where
                args2 = drop (length bind) args
                bind2 = drop (length args) bind
                bindnew = take (length bind2) (freeVars 'v' \\ collectAllVars o)
                rep = zip bind (args ++ map CoreVar bindnew)

        f x@(CoreApp (CoreLet bind xs) ys) =
                CoreLet (zip fresh (map rep rhs)) (CoreApp (rep xs) ys)
            where
                (lhs,rhs) = unzip bind
                rep = replaceFreeVars (zip fresh (map CoreVar lhs))
                fresh = freeVars 'x' \\ collectAllVars x

        f x = x



-- | Apply the Case (CoreCon ..) rule
--   This rule has a serious sharing bug (doh!)
coreSimplifyCaseCon :: CoreExpr -> CoreExpr
coreSimplifyCaseCon (CoreCase (CoreCon con) alts) = coreSimplifyCaseCon $ CoreCase (CoreApp (CoreCon con) []) alts
coreSimplifyCaseCon (CoreCase on@(CoreApp (CoreCon con) fields) alts)
        | not $ null matches = head matches
    where
        matches = mapMaybe g alts

        g (PatCon x xs, rhs) | x == con = Just $ replaceFreeVars (zip xs fields) rhs
        g (PatDefault, rhs) = Just rhs
        g _ = Nothing
coreSimplifyCaseCon x = x


-- | Apply the Case (Case ..) rule
coreSimplifyCaseCase :: CoreExpr -> CoreExpr
coreSimplifyCaseCase o@(CoreCase (CoreCase on alts1) alts2) = CoreCase on (map g alts1)
    where
        vars = freeVars 'v' \\ collectAllVars o
        g (PatCon c vs,rhs) = (PatCon c vs2, CoreCase rhs2 alts2)
            where
                vs2 = take (length vs) vars
                rhs2 = replaceFreeVars (zip vs (map CoreVar vs2)) rhs
        g (lhs,rhs) = (lhs, CoreCase rhs alts2)
coreSimplifyCaseCase x = x


-- | Apply the Case (Let ..) rule
coreSimplifyCaseLet :: CoreExpr -> CoreExpr
coreSimplifyCaseLet o@(CoreCase (CoreLet bind x) alts) =
        CoreLet (zipWith f newvars bind) (CoreCase (rep x) alts)
    where
        newvars = freeVars 'v' \\ collectAllVars o
        rep = replaceFreeVars $ zip (map fst bind) (map CoreVar newvars)
        f new (lhs,rhs) = (new, rep rhs)



uniqueExpr :: CoreExpr -> CoreExpr
uniqueExpr x = uniqueBoundVarsWithout (collectAllVars x) x


freeVars :: Char -> [String]        
freeVars c = [c:show i | i <- [1..]]


{- |
    Precondition:
    All variables must be unique

    The following patterns must not occur:

    CoreApp _ []
    CoreLet [] _
    CoreLam [] _
    CorePos _ _

    CoreCase on _ => on `notElem` {CoreCon _, CoreApp (CoreCon _) _, CoreLet _ _, CoreCase _ _}
    CoreApp x _ => x `notElem` {CoreApp _ _, CoreLet _ _, CoreCase _ _, CoreLam _ _}
    CoreLet bind _ => all (map snd bind) `notElem` {CoreLet _ _, CoreVar _}

    The following should be applied if possible (and not breaking sharing):

    CoreLet bind x => replaceFreeVars bind x
    CoreLet (CoreCase x alts) => CoreCase x (CoreLet inside each alt)
-}
coreSimplifyExprUnique :: UniqueIdM m => CoreExpr -> m CoreExpr
coreSimplifyExprUnique = coreSimplifyExprUniqueExt (const return)


{- |
    Sismplify in an extensible manner.

    @myfunc retransform@

    You should invoke retransform on all constructors you create.
-}
coreSimplifyExprUniqueExt :: UniqueIdM m => (
                                (CoreExpr -> m CoreExpr) ->
                                (CoreExpr -> m CoreExpr)
                             ) -> CoreExpr -> m CoreExpr
coreSimplifyExprUniqueExt ext = fs
    where
        fs = transformM f

        -- helpers, ' is yes, _ is no
        coreCase__ x y = f $ CoreCase x y ; coreCase_' x y = f . CoreCase x =<< y
        coreLet__  x y = f $ CoreLet  x y ; coreLet_'  x y = f . CoreLet  x =<< y
        coreLam__  x y = f $ CoreLam  x y ; coreLam_'  x y = f . CoreLam  x =<< y
        coreApp__  x y = f $ CoreApp  x y ; coreApp'_  x y = f . flip CoreApp y =<< x

        -- Simplistic transformations
        f (CorePos _ x ) = return x
        f (CoreApp x []) = return x
        f (CoreLet [] x) = return x
        f (CoreLam [] x) = return x

        -------------------------------------------------------------
        -- CASE RULES

        -- Case/Con rule
        f (CoreCase on alts) | isCoreCon con && not (null matches) = head matches
            where
                (con,fields) = fromCoreApp on
                matches = mapMaybe g alts

                g (PatDefault,rhs) = Just $ return rhs
                g (PatCon x xs, rhs) | x == fromCoreCon con = Just $ coreLet__ (zip xs fields) rhs
                g _ = Nothing

        -- Case/Case
        f (CoreCase (CoreCase on alts1) alts2) =
                coreCase_' on (mapM g alts1)
            where
                g (lhs,rhs) = do
                    CoreCase _ alts22 <- duplicateExpr $ CoreCase (CoreLit $ CoreInt 0) alts2
                    rhs2 <- coreCase__ rhs alts22
                    return (lhs,rhs2)

        -- Let's should float upwards
        f (CoreCase (CoreLet bind x) alts) =
                coreLet_' bind (coreCase__ x alts)

        -------------------------------------------------------------
        -- APP RULES
        f (CoreApp (CoreApp x xs) ys) = coreApp__ x (xs++ys)

        f (CoreApp (CoreLet bind xs) ys) =
                coreLet_' bind (coreApp__ xs ys)

        f (CoreApp (CoreCase on alts) args) = coreCase_' on (mapM g alts)
            where
                g (lhs,rhs) = do
                    args2 <- mapM duplicateExpr args
                    rhs2 <- coreApp__ rhs args2
                    return (lhs,rhs2)

        f (CoreApp (CoreLam bind x) args) =
                coreApp'_ (coreLam_' bind2 (coreLet__ (zip bind1 args1) x)) args2
            where
                m = min (length bind) (length args)

                (bind1,bind2) = splitAt m bind
                (args1,args2) = splitAt m args

        -------------------------------------------------------------
        -- LET RULES
        f (CoreLet bind (CoreCase on alts))
                | disjoint (collectFreeVars on) (map fst bind)
                = coreCase_' on (mapM g alts)
            where
                g (lhs,rhs) = do
                    rhs2 <- coreLet__ bind rhs
                    rhs3 <- duplicateExpr rhs2
                    return (lhs,rhs3)

        f (CoreLet bind x) | any (isCoreLet . snd) bind =
                coreLet_' (concat bs) $ coreLet__ vs_xs x
            where
                (vs_xs,bs) = unzip [((v,x),b) | (v,rhs) <- bind, let (b,x) = fromCoreLet rhs]

        f (CoreLet bind x) | not $ null once = coreLet_' many (fs $ replaceFreeVars once x)
            where
                bindVars = [i | CoreVar i <- concatMap (universe . snd) bind]
                (once,many) = partition (uncurry isValid) bind

                isValid lhs rhs = lhs `notElem` bindVars && (isSimple rhs || countFreeVar lhs x <= 1)
                isSimple x = isCoreFun x || isCoreVar x || (isCoreLit x && isCoreLitSmall (fromCoreLit x))

        f x = ext f x



