
module Yhc.Core.Show(showCoreExprGroup, isCoreOperator) where

import Data.List
import Data.Maybe
import Data.Char
import Yhc.Core.Type

import Text.PrettyPrint.HughesPJ



instance Show Core where
    show (Core modName depends datas funcs) =
        "module " ++ modName ++ " where\n" ++
        concatMap ("\nimport " ++) depends ++
        concatMap ("\n\n"++) (map show datas ++ map show funcs)

instance Show CoreData where
    show (CoreData name free []) = "data " ++ name ++ concatMap (' ':) free
    show (CoreData name free (x:xs)) =
        show (CoreData name free []) ++ " =\n" ++
        "      " ++ show x ++
        concatMap (("\n    | " ++) . show) xs


instance Show CoreFunc where
    show x = render $ docFunc x


instance Show CoreCtor where
    show (CoreCtor name args) = name ++ " " ++
            ['{' | useRecords] ++
            (concat $ intersperse sep $ map f args) ++
            ['}' | useRecords]
        where
            useRecords = any (isJust . snd) args
            sep = ([','|useRecords]++" ")

            f (typ, Nothing) = typ
            f (typ, Just x) = "_" ++ x ++ " :: " ++ typ


instance Show CoreExpr where
    show = render . docExpr False


inner :: Doc -> Doc
inner = nest 4

(<>>) :: Doc -> Doc -> Doc
a <>> b = sep [a, inner b]


docFunc :: CoreFunc -> Doc
docFunc (CorePrim name arity ext conv imp types) =
    text "foreign" <+> text (if imp then "import" else "export") <+> text conv <+> doubleQuotes (text ext) <+> text name <+> text "::" <+> strtype
    where
    strtype = text $ concat $ intersperse " -> " types
docFunc (CoreFunc name args body) = text initial <>> docExpr False body
    where initial = unwords (name:args) ++ " ="


-- | Show a CoreExpr, but with brackets if needed
--   so the result is a group. Does not bracket
--   simple variables or constants etc
showCoreExprGroup :: CoreExpr -> String
showCoreExprGroup = render . docExpr True


-- True is bracket, False is don't
docExpr :: Bool -> CoreExpr -> Doc
docExpr b x = f b x
    where
        -- True is do bracketing
        -- False is don't

        f b (CoreCon x) = f b (CoreVar x)
        f b (CoreFun x) = f b (CoreVar x)
        f b (CoreVar x) | isCoreOperator x = parens $ text x
                        | otherwise = text x

        f b (CorePos x y) = f b y
        f b (CoreLit x) = docLit x

        f b (CoreApp x []) = f b x
        f b (CoreApp x xs) = brack b $ call (f True x) (map (f True) xs)

        f b (CoreLam xs x) = brack b $ text ('\\' : unwords xs) <+> text "->" <+> f False x

        f b (CoreCase on alts) = brack b (text "case" <+> f True on <+> text "of" $$ inner (vcat $ map g alts))
            where
                g (a,b) = (f False (patToExpr a) <+> text "->") <>> f False b

        f b (CoreLet binds x) = brack b $ text "let" <+> vcat (map g binds) $$ text "in" <+> f False x
            where
                g (lhs,rhs) = text (lhs ++ " =") <>> f False rhs

        call x xs = sep $ x : map (nest 2) xs

brack b = if b then parens else id


docLit :: CoreLit -> Doc
docLit x = f x
    where
        f (CoreChr x) = text $ show x
        f (CoreInt x) = showNum x
        f (CoreStr x) = showNum x
        f (CoreInteger x) = showNum x
        f (CoreFloat x) = showNum x
        f (CoreDouble x) = showNum x

        showNum x = brack (head s == '-') $ text s
            where s = show x



isCoreOperator :: String -> Bool
isCoreOperator x = case dropModule x of
                       (x:_) | isAlphaNum x || x `elem` " '_([" -> False
                       _ -> True
