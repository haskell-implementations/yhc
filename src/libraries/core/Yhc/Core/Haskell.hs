
module Yhc.Core.Haskell(
    coreHaskell, coreHaskellDirect
    ) where

import Yhc.Core.Type
import Yhc.Core.Prim
import Yhc.Core.Reachable

import Data.Char
import Data.List
import Data.Maybe


-- | Take a 'Core' program, and output Haskell.
--   Fix up as much as possible
coreHaskell :: Core -> String
coreHaskell = coreHaskellDirect . coreReachable ["main"]


prefix = ["import System.IO"
         ,"import System.Environment"
         ,"import Data.Char"
         ,"import System.IO.Unsafe"
         ,"prim_FROM_STRING = map ord"
         ,"prim_FROM_CHAR = ord"
         ,"prim_TO_STRING = map chr"
         ,"prim_TO_CHAR = chr"
         ,"prim_GET_ARGS = getArgs >>= return . map prim_FROM_STRING"
         ,"main = seq (fmain ()) (return () :: IO ())"
         ,"unwrapIO :: IO a -> world -> Either () a"
         ,"unwrapIO x _ = unsafePerformIO (x >>= return . Right)"
         ,""
         ]

-- | Take a 'Core' program, and output Haskell.
--   Currently one definition per line, although this is not guaranteed (pretty printing would be nice!)
--   Does not include a /module/ definition, or imports.
coreHaskellDirect :: Core -> String
coreHaskellDirect core = unlines (prefix ++ concatMap dataHaskell (coreDatas core) ++ map funcHaskell (coreFuncs core))


dataHaskell :: CoreData -> [String]
dataHaskell (CoreData name typs ctors) 
    | name `elem` ["[]","Bool","Prelude.[]","Prelude.Bool","Prelude.(,)","(,)","Prelude.Either","Either"] = []
    | otherwise = ["data " ++ unwords (mangleData name:typs) ++ " = " ++
                   concat (intersperse " | " $ map ctorHaskell ctors)]

ctorHaskell :: CoreCtor -> String
ctorHaskell (CoreCtor name typs) = unwords (mangleCon name : map (mangleTyp . fst) typs)


funcHaskell (CoreFunc name args body) =
    unwords (mangleFun name : map mangleVar args) ++ " = " ++
    exprHaskell body


exprHaskell x =
    case x of
        CorePos _ x -> exprHaskell x
        CoreCon x -> mangleCon x
        CoreVar x -> mangleVar x
        CoreFun x -> mangleFun x
        CoreApp x xs -> "(" ++ unwords (map exprHaskell (x:xs)) ++ ")"
        CoreLam x xs -> "(\\" ++ unwords (map mangleVar x) ++ " -> " ++ exprHaskell xs ++ ")"
        
        CoreCase on alts -> "(case " ++ cast (exprHaskell on) ++ " of {" ++ concatMap f alts ++ "})"
            where
                alhs = fst $ head alts
                cast s = if isPatLit alhs then "(" ++ s ++ " :: " ++ typeConstHaskell (fromPatLit alhs) ++ ")" else s
                
                f (lhs_,rhs) = (if isCoreLit lhs then valueConstHaskell (fromCoreLit lhs) else exprHaskell lhs) ++
                              " -> " ++ exprHaskell rhs ++ " ; "
                    where lhs = patToExpr lhs_
                

        CoreLet bind x -> "(let " ++ concatMap f bind ++ " in " ++ exprHaskell x ++ ")"
            where f (lhs,rhs) = mangleVar lhs ++ " = " ++ exprHaskell rhs ++ " ; "
        
        CoreLit (CoreStr x) -> "(prim_FROM_STRING " ++ show x ++ ")"
        
        CoreLit x -> "(" ++ valueConstHaskell x ++ " :: " ++ typeConstHaskell x ++ ")"
        

typeConstHaskell x =
    case x of
        CoreInt _ -> "Int"
        CoreInteger _ -> "Integer"
        CoreChr _ -> "Int"
        CoreFloat _ -> "Float"
        CoreDouble _ -> "Double"
        

valueConstHaskell x =
    case x of
        CoreInt x -> show x
        CoreInteger x -> show x
        CoreChr x -> show $ ord x
        CoreFloat x -> show x
        CoreDouble x -> show x


primHaskell x ys = applyCast res (typs !! length ys)
    where
        res = "(" ++ unwords (op : zipWith applyCast (map exprHaskell ys) typs) ++ ")"
        typs = primType prim ++ repeat PrimTypeUnknown
        prim = corePrim x
        sop = show $ primOp prim
        op = if primOp prim == PrimDiv && head (primType prim) `elem` [PrimInt,PrimInteger] then "div"
             else if primOp prim == PrimHaskell then
                 (if primName prim == "System.Environment.getArgs" then "prim_GET_ARGS" else primName prim)
             else if primOp prim == PrimCast then casts (primType prim)
             else if isAlpha $ head sop then sop
             else "(" ++ sop ++ ")"

        casts [_,PrimInteger] = "toInteger"
        casts [PrimInteger,_] = "fromInteger"
        casts x = error $ "Do not know cast for, " ++ show x

        applyCast val t
            | t `elem` [PrimInt,PrimInteger,PrimFloat,PrimDouble] = "(" ++ val ++ " :: " ++ show t ++ ")"
            | t == PrimChar = "(prim_TO_CHAR " ++ val ++ ")"
            | t == PrimString = "(prim_TO_STRING " ++ val ++ ")"
            | otherwise = case t of
                              PrimTypeHaskell s | "IO " `isPrefixOf` s -> "(unwrapIO " ++ val ++ ")"
                              _ -> val


mangleFun = ('f':) . mangle
mangleVar = ('v':) . mangle
mangleData = ('D':) . mangle


-- important to reuse : and [], else String's don't work
mangleCon x | x == ":" || x == "Prelude.:" = "(:)"
            | x == "[]" || x == "Prelude.[]" = "[]"
            | x == "True" || x == "Prelude.True" = "True"
            | x == "False" || x == "Prelude.False" = "False"
            | x == "Left" || x == "Prelude.Left" = "Left"
            | x == "Right" || x == "Prelude.Right" = "Right"
            | x == "(,)" || x == "Prelude.(,)" = "(,)"
            | otherwise = ('C':) . mangle $ x


mangle :: String -> String
mangle x = concatMap f x
    where
        f x | isAlphaNum x = [x]
            | otherwise = '_' : show (ord x)


mangleTyp = coreDataTypeJoin . map f . coreDataTypeSplit
    where
        f x | isJust res = fromJust res
            where res = lookup x coreHaskellTypes
        f xs@(x:_) | isUpper x = mangleData xs
        f x = x
