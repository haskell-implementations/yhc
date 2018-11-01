
module Yhc.Core.Strictness(coreStrictness, coreStrictnessCustom) where

import Yhc.Core.Type
import Yhc.Core.Prim

import qualified Data.Map as Map
import Data.List(intersect, nub, partition)

{-
ALGORITHM:

SCC PARTIAL SORT:
First sort the functions so that they occur in the childmost order:
x1 < x2, if x1 doesn't transitive-call x2, and x2 does transitive-call x1
Being wrong is fine, but being better gives better results

PRIM STRICTNESS:
The strictness of the various primitive operations

BASE STRICTNESS:
If all paths case on a particular value, then these are strict in that one
If call onwards, then strict based on the caller
-}


-- | Given a function, return a list of arguments.
--   True is strict in that argument, False is not.
--   [] is unknown strictness
coreStrictness :: Core -> (CoreFuncName -> [Bool])
coreStrictness core = \funcname -> Map.findWithDefault [] funcname mp
    where mp = mapStrictness $ sccSort $ coreFuncs core


-- | Same as 'coreStrictness', but the strictness map is \"precharged\"
-- with something a custom Core converter would like to be specifically strict.
coreStrictnessCustom :: Map.Map CoreFuncName [Bool] -> Core -> (CoreFuncName -> [Bool])
coreStrictnessCustom smp core = \funcname -> Map.findWithDefault [] funcname mp
    where mp = mapStrictnessCustom smp $ sccSort $ coreFuncs core


mapStrictness :: [CoreFunc] -> Map.Map CoreFuncName [Bool]
mapStrictness funcs = mapStrictnessCustom Map.empty funcs

mapStrictnessCustom smp funcs = foldl f smp funcs
    where
        f mp (CorePrim{coreFuncName=name}) = case corePrimMaybe name of
                                    Nothing -> mp
                                    Just p -> Map.insert name (primStrict p) mp

        f mp (CoreFunc name args body) = Map.insert name (map (`elem` strict) args) mp
            where
                strict = strictVars body

                -- which variables are strict
                strictVars :: CoreExpr -> [String]
                strictVars (CorePos _ x) = strictVars x
                strictVars (CoreVar x) = [x]

                strictVars (CoreCase (CoreVar x) alts) = nub $ x : intersectList (map (strictVars . snd) alts)
                strictVars (CoreCase x alts) = strictVars x

                strictVars (CoreApp (CoreFun x) xs)
                    | length xs == length res
                    = nub $ concatMap strictVars $ map snd $ filter fst $ zip res xs
                    where res = Map.findWithDefault [] x mp

                strictVars (CoreApp x xs) = strictVars x

                strictVars _ = []


intersectList :: Eq a => [[a]] -> [a]
intersectList [] = []
intersectList xs = foldr1 intersect xs



-- do a sort in approximate SCC order
sccSort :: [CoreFunc] -> [CoreFunc]
sccSort xs = prims ++ funcs
    where (prims,funcs) = partition isCorePrim xs
