
module Yhc.Core.Reachable(coreReachable, coreReachableMap) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Yhc.Core.Type
import Yhc.Core.Uniplate


coreReachable :: [CoreFuncName] -> Core -> Core
coreReachable root = coreReachableDatas . coreReachableFuncs root


coreReachableDatas :: Core -> Core
coreReachableDatas core = core{coreDatas = filter used (coreDatas core)}
    where
        ctors = Set.fromList $ [x | CoreCon x <- universeExpr core] ++
                    [x | CoreCase _ alts <- universeExpr core, (PatCon x _,_) <- alts]

        used dat = any (`Set.member` ctors) (map coreCtorName $ coreDataCtors dat)


coreReachableFuncs :: [CoreFuncName] -> Core -> Core
coreReachableFuncs root core = fromCoreFuncMap core $ coreReachableMap root $ toCoreFuncMap core


coreReachableMap :: [CoreFuncName] -> CoreFuncMap -> CoreFuncMap
coreReachableMap root fm = f Map.empty root
    where
        f seen [] = seen
        f seen (x:xs) | x `Map.member` seen = f seen xs
                      | otherwise = f (Map.insert x func seen) (calls ++ xs)
            where
                func = coreFuncMap fm x
                calls = [y | CoreFun y <- universeExpr func]
