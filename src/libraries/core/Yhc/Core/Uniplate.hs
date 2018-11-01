
module Yhc.Core.Uniplate(
    module Yhc.Core.Uniplate,
    module Data.Generics.UniplateOn
    ) where

import Yhc.Core.Type
import Data.Generics.UniplateOn


universeExprVar :: UniplateExpr a => a -> [String]
universeExprVar x = [i | CoreVar i <- universeExpr x]


class UniplateExpr a where
    uniplateExpr :: BiplateType a CoreExpr


instance UniplateExpr a => UniplateExpr [a] where
    uniplateExpr = uniplateOnList uniplateExpr

instance UniplateExpr Core where
    uniplateExpr (Core a b c d) = (col, \ns -> Core a b c (gen ns))
        where (col,gen) = uniplateExpr d

instance UniplateExpr CoreFunc where
    uniplateExpr (CoreFunc name args body) = ([body], \[body] -> CoreFunc name args body)
    uniplateExpr x = ([], \[] -> x)

instance UniplateExpr CoreExpr where
    uniplateExpr x = ([x], \[x] -> x)


instance Uniplate CoreExpr where
    uniplate x =
        case x of
            CoreApp x xs -> (x:xs, \(n:ns) -> CoreApp n ns)
            CoreLam x xs -> ([xs], \[xs] -> CoreLam x xs)
            CorePos x xs -> ([xs], \[xs] -> CorePos x xs)

            CoreLet x xs -> (map snd x ++ [xs],
                            \ys -> CoreLet (zip (map fst x) (init ys)) (last ys))

            CoreCase x xs -> (x : map snd xs
                             ,\(y:ys) -> CoreCase y (zip (map fst xs) ys))

            _ -> ([], \[] -> x)



childrenExpr   x = childrenOn   uniplateExpr x
universeExpr   x = universeOn   uniplateExpr x
transformExpr  x = transformOn  uniplateExpr x
transformExprM x = transformOnM uniplateExpr x
rewriteExpr    x = rewriteOn    uniplateExpr x
rewriteExprM   x = rewriteOnM   uniplateExpr x
descendExpr    x = descendOn    uniplateExpr x
descendExprM   x = descendOnM   uniplateExpr x
contextsExpr   x = contextsOn   uniplateExpr x
