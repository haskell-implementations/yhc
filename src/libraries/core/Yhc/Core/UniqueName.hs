{- |
    This module implements unique names in Yhc.Core.

    Given a name, it can be dividied into [rest][digits].     
    The digits form a number (0 for no digits).

    Given a set of names, they must all represent unique numbers.
-}

module Yhc.Core.UniqueName(
    uniqueNamesNext, uniqueSplit, uniqueJoin,
    uniqueFuncsNext, uniqueFuncsSplit, uniqueFuncsRename
    ) where

import Yhc.Core.Type
import Yhc.Core.Uniplate

import Data.Char
import Control.Monad.State
import qualified Data.Map as Map


-- * General Uniqueness Functions


uniqueNamesNext :: [String] -> Int
uniqueNamesNext xs = maximum (0 : map (snd . uniqueSplit) xs) + 1


-- | Split a name into a prefix and a unique id.
--   0 means no trailing number.
uniqueSplit :: String -> (String,Int)
uniqueSplit x = (reverse b, if null a then 0 else read $ reverse a)
    where (a,b) = span isDigit $ reverse x


-- | Given a name, and a unique id, join them together.
--   Replaces any existing id.
uniqueJoin :: String -> Int -> String
uniqueJoin s n = a ++ if n == 0 then "" else show n
    where (a,b) = uniqueSplit s


-- * Those Specialised for Core

uniqueFuncsNext :: Core -> Int
uniqueFuncsNext = uniqueNamesNext . map coreFuncName . coreFuncs


type FuncsSplitM a = State FuncsSplit a
data FuncsSplit = FuncsSplit Int [CoreFunc]

-- | A more advanced combinator to capture the pattern of splitting
--   one function into many (i.e. recursive let's, lambda lifting)
--
-- Needs rank-2 types to do properly
uniqueFuncsSplit :: (
                        (FuncsSplitM CoreFuncName) ->
                        (CoreFunc -> FuncsSplitM ()) ->
                        CoreExpr -> FuncsSplitM CoreExpr
                    ) -> Core -> Core
uniqueFuncsSplit op core =
    flip evalState (uniqueFuncsNext core) $ do
        funcs <- mapM f (coreFuncs core)
        return $ core{coreFuncs = concat funcs}
    where
        newFunc name = do
            FuncsSplit j done <- get
            let name2 = uniqueJoin name j
            put $ FuncsSplit (j+1) done
            return name2

        addFunc func = do
            FuncsSplit j done <- get
            put $ FuncsSplit j (func:done)

        f x | isCorePrim x = return [x]
        f (CoreFunc name args body) = do
            i <- get
            let (body2,FuncsSplit i2 funcs2) = runState (op (newFunc name) addFunc body) (FuncsSplit i [])
            put i2
            return $ CoreFunc name args body2 : reverse funcs2


-- | Rename functions so they use consecutive numbers starting at 2,
--   to aid human understanding
uniqueFuncsRename :: Core -> Core
uniqueFuncsRename core
        | Map.null ren = core
        | otherwise = applyFuncCore g $ transformExpr f core
    where
        names = [x | CoreFunc x _ _ <- coreFuncs core, snd (uniqueSplit x) /= 0]
        ren = Map.fromList $ zip names $ zipWith uniqueJoin names [1..]

        f (CoreFun x) = CoreFun $ Map.findWithDefault x x ren
        f x = x

        g o@CoreFunc{coreFuncName=x} = o{coreFuncName = Map.findWithDefault x x ren}
        g x = x

