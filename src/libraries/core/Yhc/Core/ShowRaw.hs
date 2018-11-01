{-|
    ShowRaw is intended for debugging, to print a rather complete
    syntax tree. The basic rule is that every constructor MUST appear
    visibly in the output. For example, @show (CoreApp x []) == show x@,
    but @(showRaw x == showRaw y) ==> (x == y)@.
-}
module Yhc.Core.ShowRaw(ShowRaw(..)) where

import Yhc.Core.Type
import Data.List

g x = "(" ++ unwords x ++ ")"
s x = showRaw x

showRawList xs = "[" ++ concat (intersperse "," xs) ++ "]"


class ShowRaw a where
    showRaw :: a -> String

instance (ShowRaw a, ShowRaw b) => ShowRaw (a,b) where
    showRaw (a,b) = "(" ++ showRaw a ++ "," ++ showRaw b ++ ")"

instance ShowRaw a => ShowRaw [a] where
    showRaw xs = showRawList $ map showRaw xs


instance ShowRaw Core where
    showRaw (Core a b c d) = g ["Core", a, showRawList b, s c, s d]

instance ShowRaw CoreData where
    showRaw (CoreData a b c) = g ["Data", a, showRawList b, s c]

instance ShowRaw CoreCtor where
    showRaw (CoreCtor a b) = g ["Ctor", a, showRawList $ map f b]
        where
            f (a,Nothing) = a
            f (a,Just b) = b++"="++a

instance ShowRaw CoreFunc where
    showRaw (CoreFunc a b c) = g ["Func", a, showRawList b, s c]
    showRaw (CorePrim a b c d e f) = g ["Prim", a, show b, c, d, show e, showRawList f]

instance ShowRaw CoreExpr where
    showRaw (CoreCon a) = g ["Con", a]
    showRaw (CoreVar a) = g ["Var", a]
    showRaw (CoreFun a) = g ["Fun", a]
    showRaw (CoreApp a b) = g ("App" : s a : map s b)
    showRaw (CoreLam vs x) = g ("Lam" : vs ++ [s x])
    showRaw (CoreCase on alts) = g ["Case", s on, s alts]
    showRaw (CorePos a b) = g ["Pos",show a, s b]
    showRaw (CoreLit a) = g ["Lit",s a]
    showRaw (CoreLet vs x) = g ["Let", showRawList $ map f vs, s x]
        where f (a,b) = "(" ++ a ++ "," ++ s b ++ ")"

instance ShowRaw CoreLit where
    showRaw (CoreInt a) = g ["Int", show a]
    showRaw (CoreInteger a) = g ["Integer", show a]
    showRaw (CoreChr a) = g ["Char", show a]
    showRaw (CoreStr a) = g ["Str", show a]
    showRaw (CoreFloat a) = g ["Float", show a]
    showRaw (CoreDouble a) = g ["Double", show a]

instance ShowRaw CorePat where
    showRaw (PatCon a b) = g ("PatCon":a:b)
    showRaw (PatLit a) = g ["PatLit",s a]
    showRaw (PatDefault) = g ["PatDefault"]
