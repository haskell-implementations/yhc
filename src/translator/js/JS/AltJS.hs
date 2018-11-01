-- Core to Javascript improved conversion functionality

module JS.AltJS where

import JS.Jcode
import JS.Show
import JS.OptOpt
import JS.JGM
import Yhc.Core
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Numeric
import Control.Monad
import Control.Monad.State
import Prim

altCodeGen :: [CoreFunc] -> JGM [Jcode]

altCodeGen funcs = mapM func2JC funcs


func2JC :: CoreFunc -> JGM Jcode

func2JC func = do
  fb <- exp2JC $ remCorePos $ coreFuncBody func
  let fs = JFunction (coreFuncName func) (coreFuncArgs func) [JReturn fb]
  return fs

mkPartApp ff af = JObject [("a", JArray af),
                           ("u", ff),
                           ("e", JFunc [] (JStr "this"))]
  
mkSatApp ff af = JObject [("e", JFunc [] (JCall ff af))]

mkGenApp ff af = JObject [("e", JFunc [] (JCall (JStr "eap") [ff, JArray af]))]

exp2JC :: CoreExpr -> JGM Jexp

exp2JC (CorePos _ x) = exp2JC x 

exp2JC app@(CoreApp (CoreFun f) args) = do
  sat <- gets analSat
  sel <- gets analSel
  core <- gets coreRef
  strict <- gets analStrict
  let nargs= length args
      strcts = all id (strict f)
      arity = length $ coreFuncArgs $ coreFunc core f
      saturd = compare nargs arity
      selidx = sel app
      issel = (selidx /= -1 && nargs == 1)
  fargs <- mapM exp2JC args
  return $ case (strcts, saturd, selidx) of
    (_, LT, _) -> mkPartApp (JStr f) fargs
    (_, EQ, _) -> mkSatApp (JStr f) fargs
    (_, GT, _) | arity == 0 -> mkGenApp (JStr f) fargs
               | otherwise -> let (sat, ovf) = splitAt arity fargs
                              in mkPartApp (mkSatApp (JStr f) sat) ovf

exp2JC (CoreApp f args) = do 
  (ff:fargs) <- mapM exp2JC (f:args)
  return $ mkPartApp ff fargs


exp2JC (CoreVar v) = return $ JStr v

exp2JC (CoreFun f) = return $ JStr f

{--

exp2JC (CoreApp (CorePrim p) args) = do

exp2JC (CoreApp (CoreCon c) []) | c `elem` ["true", "false"] =
  exp2JC (CoreCon c)

exp2JC (CoreApp (CoreCon c) args) = do

exp2JC app@(CoreApp (CoreFun f) args) = do
  sat <- gets analSat
  sel <- gets analSel
  core <- gets coreRef
  strict <- gets analStrict
  let strcts = all id (strict f)
      saturd = sat app
      selidx = sel app
      issel = (selidx /= -1 && length args == 1)

exp2JC (CoreApp f args) = do


exp2JC (CoreCase exp [(CoreApp (CoreCon "true") [], iftrue), 
                       (CoreApp (CoreCon "false") [], iffalse)]) = 
  exp2JC (CoreCase exp [(CoreCon "true", iftrue), (CoreCon "false", iffalse)])

exp2JC (CoreCase exp [(CoreApp (CoreCon "false") [], iffalse),
                       (CoreApp (CoreCon "true") [], iftrue)]) =
  exp2JC (CoreCase exp [(CoreCon "true", iftrue), (CoreCon "false", iffalse)])

exp2JC (CoreCase exp [(CoreCon "true", iftrue), (CoreCon "false", iffalse)]) = do

exp2JC (CoreCase exp [f@(CoreCon "false", iffalse), t@(CoreCon "true", iftrue)]) = 
  exp2JC (CoreCase exp [t, f])

exp2JC (CoreCase exp alts) = do

exp2JC (CoreLet lets exp) = do

exp2JC ex = 
--}

exp2JC _ = return $ JStr "/* ??? */"
