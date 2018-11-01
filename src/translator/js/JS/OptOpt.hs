-- Optimization Options and core analysis functions

module JS.OptOpt where

import Yhc.Core
import Data.Maybe
import Data.List

data CodeGen = StdCodeGen | AltCodeGen

data JSOptFlags = JSOF {
   oKeepStrIdx   :: Bool -- preserve string index
  ,oOptSaturated :: Bool -- optimize for saturated calls
  ,oOptSelectors :: Bool -- optimize for selector function calls
  ,oOptStrict    :: Bool -- use the strictness analyzer
  ,oProfiling    :: Bool -- profile execution
  ,oCaseElim     :: Bool -- run case eliminator
  ,oCoreSimplify :: Bool -- run Core simplifier
  ,oCoreInline   :: InlineMode -- how to apply core inlining
  ,oRecursiveLet :: Bool -- run removeRecursiveLet
  ,oApplyLimit   :: Int  -- limit of constructor arity to use apply in pattern matching
  ,oArityAlias   :: Bool -- create aliases for functions' partial applications
  ,oCodeGen      :: CodeGen -- which code generation method to use
}

defJSOF = JSOF {
   oKeepStrIdx = True
  ,oOptSaturated = True
  ,oOptSelectors = True
  ,oOptStrict = True
  ,oProfiling = False
  ,oCaseElim = False
  ,oCoreSimplify = True
  ,oCoreInline = InlineForward -- InlineAlias
  ,oRecursiveLet = True
  ,oApplyLimit = 5
  ,oArityAlias = False
  ,oCodeGen = StdCodeGen
}

-- | Given an expr (normally a CoreApp)
--   say if it is an application of a selector function
--   to a data object. Selector functions consist of a single
--   CoreCase statement with the only alternative. Application
--   must be exactly to one argument. The case alternative must
--   be a constructor application to field selectors, and 
--   the return value must be one of the selectors.
--   If the analysis condition is satisfied, a field index is returned.
--   Otherwise -1 is returned.

coreSelectorApp :: Core -> (CoreExpr -> Int)

coreSelectorApp core = 
  \x -> case x of
    CoreApp (CoreFun x) [y] -> f x y
    _ -> -1
  where
    unpos (CorePos _ e) = unpos e
    unpos e = e
    f x y = case coreFuncMaybe core x of
      (Just func@CoreFunc {coreFuncArgs = (a:[])}) -> 
        case unpos (coreFuncBody func) of
          CoreCase _ [(PatCon con sels, (CoreVar ce))] -> 
            fromMaybe (-1) (elemIndex ce sels)
          _ -> -1
      _ -> -1

