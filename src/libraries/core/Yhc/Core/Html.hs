
module Yhc.Core.Html(coreHtml) where

import Yhc.Core.Type
import Yhc.Core.Show(isCoreOperator)
import Yhc.Core.Internal.HughesPJ

import Data.List
import Data.Maybe
import Data.Char


prefix modu =
    "<html>" ++
         "<head>" ++
             "<style type='text/css'>" ++
             "body {font-family: monospace; white-space: pre;}" ++
             ".op {color:red;}" ++
             ".key {color:blue;}" ++
             ".str {color:teal;}" ++
             "div {border: 3px solid white;}" ++
             "a {text-decoration: none; color: black;}" ++
             "</style>" ++
             "<script>" ++ script ++ "</script>" ++
             "<title>" ++ modu ++ " - Yhc.Core</title>" ++
         "</head>" ++
         "<body>\n"

suffix = "\n</body></html>"


script =
    "var done = '';\n" ++
    "\n" ++
    "function none()\n" ++
    "{\n" ++
    "   if (done == '') return;\n" ++
    "   done = '';\n" ++
    "   document.styleSheets[0].deleteRule(0);\n" ++
    "   document.styleSheets[0].deleteRule(0);\n" ++
    "}\n" ++
    "\n" ++
    "function use(node)\n" ++
    "{\n" ++
    "   if (done == node)\n" ++
    "   {\n" ++
    "       none();\n" ++
    "       return false;\n" ++
    "   }\n" ++
    "\n" ++
    "   var n = document.getElementById(node);\n" ++
    "   var offsetBottom = n.offsetTop + n.offsetHeight;\n" ++
    "   var scrollBottom = document.body.scrollTop + window.innerHeight;\n" ++
    "\n" ++
    "   def(node);\n" ++
    "\n" ++
    "   return (n.offsetTop < document.body.scrollTop || offsetBottom > scrollBottom)\n" ++
    "}\n" ++
    "\n" ++
    "function def(node)\n" ++
    "{\n" ++
    "   if (done == node)\n" ++
    "   {\n" ++
    "       none();\n" ++
    "       return;\n" ++
    "   }\n" ++
    "\n" ++
    "   none();\n" ++
    "   var rule1 = '#' + node + '{border-color: #ff4;}'\n" ++
    "   var rule2 = '.' + node + '{background-color: #ff4;}'\n" ++
    "\n" ++
    "   document.styleSheets[0].insertRule(rule1,0);\n" ++
    "   document.styleSheets[0].insertRule(rule2,0);\n" ++
    "   done = node;\n" ++
    "}\n" ++
    "\n"



coreHtml :: Core -> String
coreHtml core = prefix (coreName core) ++ show (docCore core) ++ suffix



listLines = vcat -- . intersperse (text "\n")
blankLine = text ""


wrap prepend doc append = zeroText prepend <> doc <> zeroText append
tag s x = wrap ("<span class='" ++ s ++ "'>") (text x) "</span>"

enc = concatMap f
    where
        f x | isAlphaNum x = [x]
            | otherwise = show (ord x)

key = tag "key"
op = tag "op"
str = tag "str"


opchars = hcat . map f
    where
        f x | x `elem` "[]()," = op [x]
            | otherwise = char x


docCore :: Core -> Doc
docCore core@(Core modName depends datas funcs) = listLines $
        [key "module" <+> text modName <+> key "where", blankLine] ++
        map ((key "import" <+>) . text) depends ++ [blankLine] ++
        intersperse (blankLine <> blankLine) (map docData datas ++ map (docFunc core) funcs)


docData :: CoreData -> Doc
docData (CoreData name free []) = key "data" <+> hsep (map text (name:free))
docData (CoreData name free (x:xs)) =
        docData (CoreData name free []) <+> op "=" <+> text "\n" <+>
        text "      " <> docCtor x <+>
        hsep (map (\x -> text "\n    " <> op "|" <> text " " <> docCtor x) xs)


docCtor :: CoreCtor -> Doc
docCtor (CoreCtor name args) = text name <+> text (
            ['{' | useRecords] ++
            (concat $ intersperse sep $ map f args) ++
            ['}' | useRecords])
        where
            useRecords = any (isJust . snd) args
            sep = ([','|useRecords]++" ")

            f (typ, Nothing) = typ
            f (typ, Just x) = "_" ++ x ++ " :: " ++ typ



inner :: Doc -> Doc
inner = nest 4

(<>>) :: Doc -> Doc -> Doc
a <>> b = sep [a, inner b]



docFunc :: Core -> CoreFunc -> Doc
docFunc core x = wrap ("<div id='" ++ ename ++ "'>") res "</div>"
    where
        res = body initial

        ename = enc name
        name = coreFuncName x
        body = if isCoreFunc x then (<>> docExpr core (coreFuncBody x)) else id
        args = if isCoreFunc x then hsep (map text (coreFuncArgs x)) <+> op "="
                               else text $ "arity=" ++ show (corePrimArity x)
        prefix = if isCoreFunc x then text name
                                 else key "primitive" <+> text name

        pre = "<a name='" ++ ename ++ "'></a><a href='javascript:def(\"" ++ ename ++ "\")'>"
        initial = wrap pre prefix "</a>" <+> args


-- True is bracket, False is don't
docExpr :: Core -> CoreExpr -> Doc
docExpr core x = f False x
    where
        -- True is do bracketing
        -- False is don't

        f b (CoreCon x) = f b (CoreVar x)
        f b (CoreFun x) | not link = f b (CoreVar x)
                        | otherwise = wrap pre (f b (CoreVar x)) "</a>"
            where
                pre = "<a class='" ++ ename ++ "' href='#" ++ ename ++ "' onclick='return use(\"" ++ ename ++ "\")'>"
                link = isJust $ coreFuncMaybe core x
                ename = enc x

        f b (CoreVar x) = brack (isCoreOperator x) (opchars x)

        f b (CoreLam xs x) = brack b $ char '\\' <> text (unwords xs) <+> text "->" <+> f False x

        f b (CoreLit x) = g x

        f b (CorePos x y) = f b y

        f b (CoreApp x []) = f b x
        f b (CoreApp x xs) = brack b $ call (f True x) (map (f True) xs)

        f b (CoreCase on alts) = brack b (key "case" <+> f True on <+> key "of" $$ inner (vcat $ map g alts))
            where
                g (a,b) = (f False (patToExpr a) <+> op "->") <>> f False b

        f b (CoreLet binds x) = brack b $ key "let" <+> vcat (map g binds) $$ key "in" <+> f False x
            where
                g (lhs,rhs) = text lhs <+> op "=" <>> f False rhs

        call x xs = sep $ x : map (nest 2) xs
        brack b x = if b then op "(" <> x <> op ")" else x

        g (CoreInt x) = text $ show x
        g (CoreChr x) = str $ show x
        g (CoreStr x) = str $ show x
        g (CoreInteger x) = text $ show x
        g (CoreFloat x) = text $ show x
        g (CoreDouble x) = text $ show x
