
module Yhc.Core.CaseElimination(coreCaseElim) where

import Yhc.Core.Type
import Yhc.Core.Uniplate

import Data.List((\\))


-- | Eliminate useless default statements
--   where the other options cover everything
coreCaseElim :: Core -> Core
coreCaseElim core = transformExpr f core
    where
        coreSets = map (map coreCtorName . coreDataCtors) (coreDatas core)


        f (CoreCase on alts)
                | not (null cons) && not (null cors) && null (cors1 \\ cons)
                = CoreCase on (filter (not . isPatDefault . fst) alts)
            where
                cors = filter (cons1 `elem`) coreSets
                cons = [x | (PatCon x _, _) <- alts]
                (cors1,cons1) = (head cors, head cons)

        f x = x
