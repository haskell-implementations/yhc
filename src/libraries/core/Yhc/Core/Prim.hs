
module Yhc.Core.Prim(
    Prim(..), PrimOp(..), PrimType(..), primArity,
    corePrims, corePrim, corePrimMaybe,
    coreBytecodePrims, coreHaskellPrims, coreHaskellTypes
    ) where

import Yhc.Core.Type
import Data.List
import Data.Maybe

import qualified Data.Map as Map
import qualified Data.Set as Set


data PrimType = PrimTypeAny | PrimTypeUnknown | PrimTypeHaskell String | PrimIO
              | PrimInt | PrimInteger | PrimDouble | PrimFloat | PrimChar | PrimString | PrimBool
                deriving (Eq, Ord)

instance Show PrimType where
    show x = case x of
                 PrimTypeAny -> "*"
                 PrimTypeUnknown -> "?"
                 PrimTypeHaskell s -> s ++ "#"
                 PrimInt -> "Int"
                 PrimInteger -> "Integer"
                 PrimDouble -> "Double"
                 PrimFloat -> "Float"
                 PrimChar -> "Char"
                 PrimString -> "String"
                 PrimBool -> "Bool"


data Prim = Prim
    {primName :: String
    ,primType :: [PrimType]  -- | a function signature
    ,primStrict :: [Bool] -- | True is strict in argument n, [] is unknown
    ,primEval :: [CoreExpr] -> CoreExpr
    ,primOp :: PrimOp
    }

primArity :: Prim -> Int
primArity = length . tail . primType


instance Show Prim where
    show (Prim name typ strict _ other) = name ++ " :: " ++ t ++ " -- " ++ show other
        where
            t = concat $ intersperse " -> " $ zipWith f (strict ++ repeat False) typ
            f s x = ['!'|s] ++ show x


data PrimOp = PrimSeq | PrimOrd
            | PrimAdd | PrimSub | PrimMul
            | PrimDiv | PrimRem | PrimQuot | PrimQuotRem
            | PrimNeg | PrimAbs | PrimSignum
            | PrimEq | PrimNe | PrimLt | PrimGt | PrimLe | PrimGe
            | PrimCast | PrimHaskell | PrimOther String
            deriving Eq

instance Show PrimOp where
    show (PrimOther x) = x
    show x = fromMaybe (error "here") $ lookup x table
        where
            table = [(PrimSeq,"seq")
                    ,(PrimAdd,"+"),(PrimSub,"-"),(PrimMul,"*")
                    ,(PrimDiv,"/"),(PrimRem,"rem"),(PrimQuot,"quot"),(PrimQuotRem,"quotRem")
                    ,(PrimEq,"=="),(PrimNe,"/="),(PrimLt,"<"),(PrimGt,">"),(PrimLe,"<="),(PrimGe,">=")
                    ,(PrimCast,"cast")
                    ,(PrimNeg,"negate"),(PrimAbs,"abs"),(PrimSignum,"signum")
                    ,(PrimHaskell,"Haskell")]


corePrims :: [Prim]
corePrims = coreBytecodePrims ++ coreHaskellPrims


coreBytecodePrims :: [Prim]
coreBytecodePrims =
            [Prim "SEQ" [PrimTypeAny,PrimTypeAny,PrimTypeAny] [True,True] undefined PrimSeq
            ,Prim "ORD" [PrimTypeAny,PrimInt] [True] undefined PrimOrd
            ,add "ADD_W" PrimInt, add "YHC.Primitive.primIntegerAdd" PrimInteger
            ,sub "SUB_W" PrimInt, sub "YHC.Primitive.primIntegerSub" PrimInteger
            ,neg "NEG_W" PrimInt, neg "YHC.Primitive.primIntegerNeg" PrimInteger
            ,abs "YHC.Primitive.primIntAbs" PrimInt
            ,signum "YHC.Primitive.primIntSignum" PrimInt
            ,mul "MUL_W" PrimInt, mul "YHC.Primitive.primIntegerMul" PrimInteger
            ,div "SLASH_D" PrimDouble, div "SLASH_F" PrimFloat
            ,rem "REM" PrimInt, rem "YHC.Primitive.primIntegerRem" PrimInteger
            ,quot "QUOT" PrimInt, quot "YHC.Primitive.primIntegerQuot" PrimInteger
            ,quotRem "YHC.Primitive.primIntegerQuotRem" PrimInteger
            ,eq "EQ_W" PrimInt, eq "YHC.Primitive.primIntegerEq" PrimInteger, eq "EQ_F" PrimFloat
            ,ne "NE_W" PrimInt, ne "YHC.Primitive.primIntegerNe" PrimInteger, ne "NE_F" PrimFloat
            ,lt "LT_W" PrimInt, lt "YHC.Primitive.primIntegerLt" PrimInteger, lt "LT_F" PrimFloat
            ,le "LE_W" PrimInt, le "YHC.Primitive.primIntegerLe" PrimInteger, le "LE_F" PrimFloat
            ,gt "GT_W" PrimInt, gt "YHC.Primitive.primIntegerGt" PrimInteger, gt "GT_F" PrimFloat
            ,ge "GE_W" PrimInt, ge "YHC.Primitive.primIntegerGe" PrimInteger, ge "GE_F" PrimFloat
            ,cast "YHC.Primitive.primDoubleFromInteger" PrimInteger PrimDouble
            ,cast "YHC.Primitive.primIntFromInteger" PrimInteger PrimInt
            ,cast "YHC.Primitive.primIntegerFromInt" PrimInt PrimInteger
            ,ne "NE_D" PrimDouble, eq "EQ_D" PrimDouble
            ,lt "LT_D" PrimDouble, le "LE_D" PrimDouble
            ,gt "GT_D" PrimDouble, ge "GE_D" PrimDouble
            ,mul "MUL_D" PrimDouble, add "ADD_D" PrimDouble, sub "SUB_D" PrimDouble,neg "NEG_D" PrimDouble
            ,mul "MUL_F" PrimFloat , add "ADD_F" PrimFloat , sub "SUB_F" PrimFloat ,neg "NEG_F" PrimFloat
            ]
    where
        add = trip PrimAdd; sub = trip PrimSub; mul = trip PrimMul;
        div = trip PrimDiv; rem = trip PrimRem; quot = trip PrimQuot
        quotRem = tup PrimQuotRem
        eq = comp PrimEq; ne = comp PrimNe; lt = comp PrimLt; gt = comp PrimGt
        le = comp PrimLe; ge = comp PrimGe
        neg = one PrimNeg; abs = one PrimAbs; signum = one PrimSignum
        
        trip symbol name typ = Prim name [typ,typ,typ] [True,True] undefined symbol
        comp symbol name typ = Prim name [typ,typ,PrimBool] [True,True] undefined symbol
        one  symbol name typ = Prim name [typ,typ] [True] undefined symbol
        tup  symbol name typ = Prim name [typ,typ,PrimTypeUnknown] [True,True] undefined symbol
        cast name from to = Prim name [from,to] [True] undefined PrimCast


corePrim :: String -> Prim
corePrim s = fromMaybe (error $ "Yhc.Core.Prim.corePrim, could not find primitive: " ++ s) $ corePrimMaybe s


corePrimMaybe :: String -> Maybe Prim
corePrimMaybe search = listToMaybe [x | x <- corePrims, primName x == search]


coreHaskellPrims :: [Prim]
coreHaskellPrims =
    [hask "System.IO.stdout" [handle]
    ,hask "System.IO.stderr" [handle]
    ,hask "System.IO.stdin" [handle]
    ,hask "System.IO.hPutChar" [handle,PrimChar,io]
    ,hask "Prelude.putChar" [PrimChar,io]
    ,hask "Prelude.getChar" [PrimTypeHaskell "IO Char"]
    ,hask "System.Environment.getArgs" [PrimTypeHaskell "IO [String]"]
    ,hask "Prelude.error" [PrimString, PrimTypeAny]
    ,Prim "Prelude.strError" [] [] undefined (PrimOther "show")
    ]
    where
        handle = PrimTypeHaskell "System.IO.Handle"
        io = PrimTypeHaskell "IO ()"
        hask name typs = Prim name typs [] undefined PrimHaskell


coreHaskellTypes :: [(String, String)]
coreHaskellTypes =
    [("YHC.Primitive.Handle", "System.IO.Handle")
    ,("Prelude.Char","Prelude.Int")
    ,("Prelude.Int","Prelude.Int")
    ,("Prelude.String","[Prelude.Char]")
    ]
