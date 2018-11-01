
module Yhc.Core.Clean(
    coreClean
    ) where

import Yhc.Core.Type

import Data.Char
import Data.List


-- | Take a 'Core' program, and output Clean.
--   Currently one definition per line, although this is not guaranteed (pretty printing would be nice!)
--   Does not include a /module/ definition, or imports.
coreClean :: Core -> String
coreClean core = unlines (concatMap dataClean (coreDatas core) ++ map funcClean (coreFuncs core))


-- :: Bool = True | False
dataClean :: CoreData -> [String]
dataClean (CoreData name typs ctors) 
    | name `elem` ["[]","Bool","Prelude.[]","Prelude.Bool"] = []
    | otherwise = [":: " ++ unwords (mangleData name:typs) ++ " = " ++
                   concat (intersperse " | " $ map ctorClean ctors)]

ctorClean :: CoreCtor -> String
ctorClean (CoreCtor name typs) = unwords (mangleCon name : map (mangleTyp . fst) typs)


funcClean (CoreFunc name args body) =
    unwords (mangleFun name : map mangleVar args) ++ " = " ++
    exprClean body


exprClean x =
    case x of
        CorePos _ x -> exprClean x
        CoreCon x -> mangleCon x
        CoreVar x -> mangleVar x
        CoreFun x -> mangleFun x
        CoreApp x xs -> "(" ++ unwords (map exprClean (x:xs)) ++ ")"
        CoreLam x xs -> "(\\" ++ unwords (map mangleVar x) ++ " -> " ++ exprClean xs ++ ")"
        
        CoreCase on alts -> "(case " ++ exprClean on ++ " of {" ++ concatMap f alts ++ "})"
            where f (lhs,rhs) = exprClean (patToExpr lhs) ++ " -> " ++ exprClean rhs ++ " ; "

        CoreLet bind x -> "(let " ++ concatMap f bind ++ " in " ++ exprClean x ++ ")"
            where f (lhs,rhs) = mangleVar lhs ++ " = " ++ exprClean rhs ++ " ; "
        
        CoreLit x -> litClean x


litClean x =
    case x of
        CoreInt x -> "(" ++ show x ++ ")"
        CoreInteger x -> "(" ++ show x ++ ")"
        CoreChr x -> show x
        CoreStr x -> show x
        CoreFloat x -> "(" ++ show x ++ ")"
        CoreDouble x -> "(" ++ show x ++ ")"


mangleFun = ('f':) . mangle
mangleVar = ('v':) . mangle
mangleData = ('D':) . mangle


-- important to reuse : and [], else String's don't work
mangleCon x | x == ":" || x == "Prelude.:" = "(:)"
            | x == "[]" || x == "Prelude.[]" = "[]"
            | x == "True" || x == "Prelude.True" = "True"
            | x == "False" || x == "Prelude.False" = "False"
            | otherwise = ('C':) . mangle $ x


mangle :: String -> String
mangle x = concatMap f x
    where
        f x | isAlphaNum x = [x]
            | otherwise = '_' : show (ord x)


mangleTyp x = "(" ++ unwords (map f $ words x) ++ ")"
    where
        f x | x == "Prelude.Char" = "Int"
        f xs@(x:_) | isUpper x = mangleData xs
        f x = x
