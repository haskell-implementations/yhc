
module Yhc.Core.Invariant(
    Invariant(..),
    checkInvariant, ensureInvariant,
    checkInvariants, ensureInvariants
    ) where

import Data.List
import Data.Maybe

import Yhc.Core.Type
import Yhc.Core.Uniplate
import Yhc.Core.UniqueName
import Yhc.Core.FreeVar3

import Yhc.Core.RecursiveLet
import Yhc.Core.Invariant.LambdaLift


-- | Note, not all combinations are yet implemented - they crash at runtime.
--   If you want any invariant, just email the list.
data Invariant
    -- Local and reasonably syntactic
    = NoCoreLet -- ^ The CoreLet constructor must not occur. Removal reduces sharing
    | NoCorePos -- ^ The CorePos constructor must not occur.
    | CoreAppFun -- ^ All CoreFun's must be enclosed in a CoreApp.
    | CoreAppCon -- ^ All CoreCon's must be enclosed in a CoreApp.
    | NoEmptyApp -- ^ All CoreApp's must not have an empty argument list.
    | CoreCaseVar -- ^ All CoreCase's must be on a variable.
    | NoCaseDefault -- ^ All constructor CoreCase's must not contain a default.
    | NoCaseDefaultOne -- ^ All constructor CoreCase defaults must represent at least two constructors.
    | NoCaseConst -- ^ All CoreCase's must be on constructors, not constants.

    -- Requires new functions to be created
    | NoRecursiveLet -- ^ CoreLet's must not be recursive. Removal reduces sharing in limited cases
    | NoCoreLam -- ^ The CoreLam constructor must not occur.
    | NoPartialAppPrim -- ^ No partial applications of CoreFun to a CorePrim
    | NoPartialAppCon -- ^ No partial applications of CoreCon

    -- Uniqueness and Normal Form
    | ConsecutiveFuncs -- ^ Low function numbers
    | UniqueVarsFunc -- ^ Unique variables in each function
    | UniqueVarsCore -- ^ Unique variables in the whole program
    
    -- Global
    | FuncArityAtMostOne -- ^ All CoreApp CoreFun's must have at most one argument directly present
    | FuncArityExactlyOne -- ^ All CoreApp CoreFun's must have exactly one argument present
    deriving (Eq,Show,Enum,Bounded)


---------------------------------------------------------------------
-- * Check Invariants

checkInvariants :: [Invariant] -> Core -> Bool
checkInvariants is = null . failingInvariants is


failingInvariants :: [Invariant] -> Core -> [Invariant]
failingInvariants is core = filter (not . flip checkInvariant core) is


checkInvariant :: Invariant -> Core -> Bool
checkInvariant = flip check


check core NoCoreLet = not $ any isCoreLet $ universeExpr core
check core NoCoreLam = not $ any isCoreLam $ universeExpr core
check core NoCorePos = not $ any isCorePos $ universeExpr core

check core NoRecursiveLet = not $ any isCoreLetRec $ universeExpr core

check core ConsecutiveFuncs = f ids
    where
        f (i:j:is) | i == j || i+1 == j = f (j:is)
        f is = length is <= 1

        ids = sort [i | func <- coreFuncs core, isCoreFunc func
                      , let i = snd $ uniqueSplit $ coreFuncName func, i > 2]

check core NoCaseDefaultOne = True -- skip for now!

check core x = error $ "Yhc.Core.checkInvariant: Not yet implemented, " ++ show x


---------------------------------------------------------------------
-- * Ensure Invariants

-- specifying more than one invariant from any pool is an error
conflicts = [[CoreAppFun, NoEmptyApp], [CoreAppCon, NoEmptyApp]
            ,[NoCaseDefault, NoCaseDefaultOne], [FuncArityAtMostOne, FuncArityExactlyOne]
            ,[CoreCaseVar, NoCoreLet]
            ]

-- return true if there are any invariants
anyConflicts :: [Invariant] -> Bool
anyConflicts is = any ((> 1) . length . intersect is) conflicts


-- which invariants require an additional one to be inserted before
requires = [(NoCoreLet, [NoRecursiveLet])]

addRequires :: [Invariant] -> [Invariant]
addRequires is = is ++ concatMap (fromMaybe [] . flip lookup requires) is


-- anything not specified should be done afterwards
order = concat [
            [NoRecursiveLet, NoCoreLam]
            ]

bestOrder :: [Invariant] -> [Invariant]
bestOrder is = filter (`elem` is) items
    where items = order ++ ([minBound..maxBound] \\ order)


validate :: [Invariant] -> Core -> Core
validate is c | null fails = c
              | otherwise = error $ "Yhc.Core.ensureInvariants: BRAIN EXPLODED! " ++ show fails
    where fails = failingInvariants is c

ensureInvariant :: Invariant -> Core -> Core
ensureInvariant i = ensureInvariants [i]


ensureInvariants :: [Invariant] -> Core -> Core
ensureInvariants is core
    | anyConflicts is = error $ "Yhc.Core.ensureInvariants: conflicting invariants\n" ++ show is
    | otherwise = validate is $ foldl ensure core $ bestOrder $ addRequires is


ensure core NoRecursiveLet = removeRecursiveLet core
ensure core NoCoreLam = coreLambdaLift core

ensure core NoCorePos = transformExpr remCorePos core

ensure core NoCoreLet = transformExpr f core
    where
        f (CoreLet bind x) = replaceFreeVars bind x
        f x = x

ensure core NoCaseDefault = caseRemoveDefault True core
ensure core NoCaseDefaultOne = caseRemoveDefault False core

ensure core ConsecutiveFuncs = uniqueFuncsRename core

ensure core x = error $ "Yhc.Core.ensureInvariant: Not yet implemented, " ++ show x


---------------------------------------------------------------------
-- ** Case Default Removal


caseRemoveDefault :: Bool -> Core -> Core
caseRemoveDefault alls core = transformExpr f core
    where
        check 0 = True
        check n = alls || n == 1
    
        f (CoreCase on alts)
                | length alts > 1 && isPatDefault deflhs && not (null seen) &&
                  isJust dat && check (length missing)
                = CoreCase on $ init alts ++ concatMap g (coreDataCtors $ fromJust dat)
            where
                (deflhs,defrhs) = last alts
                missing = map coreCtorName (coreDataCtors $ fromJust dat) \\ seen
                dat = coreCtorDataMaybe core (head seen)
                seen = [c | (PatCon c _,_) <- alts]
                free = freeVars 'v' \\ collectAllVars defrhs

                g c | name `notElem` seen = [(PatCon name vars, defrhs)]
                    where
                        vars = take (length $ coreCtorFields c) free
                        name = coreCtorName c
                g _ = []

        f x = x
