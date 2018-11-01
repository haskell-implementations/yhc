-- Core to Javascript conversion functionality

module JS.CoreJS where

import JS.Jcode
import JS.Show
import JS.OptOpt
import JS.AltJS
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

-- A stub function always returning False used to turn
-- certain optimizations off

noOpt :: a -> (b -> a)
noOpt a = (\_ -> a)

-- Force an expression evaluation: strict expressions do not need extra ._c() method
-- called upon them to evaluate.

evalForce :: Jexp -> JGM Jexp

-- evalForce ep@(JPrim p a) = ep
-- evalForce ei@(JInfix s l r) = ei
evalForce en@(JNum s) = return en
evalForce e = return $ JCall (JStr "exprEval") [e]

allFalse = map (== 0) [1 ..]

core2JS :: JSOptFlags -> Core -> [Jcode]

core2JS flg core = 
  let env = JG {stateCnt = 1,
                coreRef = core, 
                optOpt = flg,
                analSat = if (oOptSaturated flg) then (coreSaturated core) else noOpt False,
                analSel = if (oOptSelectors flg) then (coreSelectorApp core) else noOpt (-1),
                analStrict = if (oOptStrict flg) 
                  then (coreStrictness core) 
                  else noOpt allFalse} in
  flip evalState env $ do 
    core <- gets coreRef
    flg <- gets optOpt
    case oCodeGen flg of
      StdCodeGen -> do
        jc <- mapM func2JS (coreFuncs core)
        return $ concat jc
      AltCodeGen -> altCodeGen (coreFuncs core)

func2JS :: CoreFunc -> JGM [Jcode]

-- For the "unsafeJS" function, its String argument is located.
-- This argument is retrieved and wrapped into a function header.
-- All others are processed by another function.

func2JS func | isCorePrim func = return []

func2JS func = do
  core <- gets coreRef
  flg <- gets optOpt
  let name = coreFuncName func
      args = coreFuncArgList func
      body = coreFuncBody func
  case body of
    CorePos _ e -> func2JS func {coreFuncBody = e}
    CoreApp (CoreFun ujsm) (cvstr:_) | isCorePrim (coreFunc core ujsm) -> do
      let ujs = dropModule ujsm
      case ujs of 
        'u':'n':'s':'a':'f':'e':'J':'S':_ -> do
          let jstr = case cvstr of
                CoreApp (CoreFun s) _ -> s
                other -> error $ "cannot process the argument of " ++ 
                                 ujs ++ ": " ++ show cvstr
              estr1 = "Function " ++ name ++ " called " ++ ujs
          case (coreFuncMaybe core jstr) of
            Nothing -> error $ estr1 ++
                               " which was looking for " ++ jstr ++ 
                               " but could not find it within module " ++ coreName core
            Just func'@(CoreFunc {coreFuncArgs = []}) -> do
              let body' = coreFuncBody func'
              case body' of
                CorePos _ (CoreLit (CoreStr ujstring)) -> do
                  let jsraw = JRaw ujstring
                      profb = JRaw $ 
                        strJexp (JCall (JStr "profile") [JStr "'body_js'", 
                                                         JStr ("'" ++ name ++ "'")]) ++ ";"
                      jargs = map (\x -> [x]) (take (length args) ['a', 'b' ..])
                      arity = JStr $ show $ length args
                      body = JProc jargs $ case oProfiling flg of
                        False -> [jsraw]
                        True -> [profb, jsraw]
                      funtp = JCall (JStr "new HSFun") [JStr $ show name, arity, body]
                  return [JAssignV (JStr name) funtp]
                other -> error $ estr1 ++
                                 " which must be called with a string literal, but" ++
                                 " is called with\n" ++ show body'
            other -> error $ estr1 ++
                             " which in turn calls " ++ jstr ++ " expected to be " ++
                             " of zero arity, but indeed expects some arguments"

        other -> selFunc2JS func
    other -> selFunc2JS func

-- A function showing whether a Core function definition may be eliminated
-- and replaced with the value function returns.
  
funcEliminable :: JSOptFlags -> CoreFunc -> Bool
      
funcEliminable jsof cf =  
  case coreFuncArgList cf of 
    [] -> case coreFuncBody cf of
      CorePos _ b -> funcEliminable jsof cf {coreFuncBody = b}
      CoreLit (CoreStr _) -> True    -- nullary function returning a string literal
      other -> False
    other -> False

-- A shortcut for selector function body optimization.

selFunc2JS :: CoreFunc -> JGM [Jcode]

selFunc2JS func = do
  jsof <- gets optOpt
  sel <- gets analSel
  let name = coreFuncName func
      args = coreFuncArgList func
      unpos (CorePos _ e) = unpos e
      unpos e = e
  case unpos (coreFuncBody func) of
    CoreCase _ [(PatCon con sels, ce)] -> do
      case (length args == 1) && (oOptSelectors jsof) of
        False -> regFunc2JS func
        True -> do
          let selidx = sel (CoreApp (CoreFun name) [CoreVar "_"])
          case selidx of
            (-1) -> regFunc2JS func 
            _ -> do
              af <- evalForce (JStr (head args)) 
              let arity = JStr $ "1"
                  jsexp = JIndex (JMember "_f" af) (JStr $ show selidx)
                  body = JFunc [head args] jsexp
                  funtp = JCall (JStr "new HSFun") [JStr $ show name, arity, body]
              return [JAssignV (JStr name) funtp]
    _ -> regFunc2JS func

regFunc2JS :: CoreFunc -> JGM [Jcode]

-- An eliminable function may be replaced by its body.
-- For a regular function, a template object is created
-- which is evaluated by its method which looks at the number of arguments
-- remaining and either returns the object itself or calls function body with
-- arguments.

regFunc2JS func = do
  let name = coreFuncName func
      args =  coreFuncArgList func
      fbody = coreFuncBody func
  jsexp <- exp2JS fbody
  jsof <- gets optOpt
  case (funcEliminable jsof func) of
    True -> return [JAssignV (JStr name) jsexp]
    False -> do
      strict <- gets analStrict
      let strct = take (length args) (strict name)
      let arity = JStr $ show $ length args
          profb = JCall (JStr "profile") [JStr "'body_hs'", JStr ("'" ++ name ++ "'")]
          body  = JFunc args $ case oProfiling jsof of
            False -> jsexp
            True -> JComma profb jsexp
          funtp = JCall (JStr "new HSFun") [JStr $ show name, arity, body]
      return $ case (".unsafeJS" `isSuffixOf` name) of
        True ->  []
        False -> [JAssignV (JStr name) funtp]


exp2JSns :: CoreExpr -> JGM Jexp

exp2JSns ce = do
  oldst <- get
  let newst = oldst {
     analStrict = noOpt allFalse
   , analSat = noOpt False
  }
  put newst
  e <- exp2JS ce
  put oldst
  return e

exp2JS :: CoreExpr -> JGM Jexp

exp2JS (CorePos s e) = exp2JS e

exp2JS (CoreVar s) = return $ JStr s
exp2JS (CoreFun s) = return $JStr s

exp2JS (CoreLit (CoreInt i)) = return $ JNum (show i)
exp2JS (CoreLit (CoreInteger i)) = return $ JNum (show i)
exp2JS (CoreLit (CoreFloat i)) = return $ JNum (show i)
exp2JS (CoreLit (CoreDouble i)) = return $ JNum (show i)

exp2JS (CoreLit (CoreChr c))  = return $ JCall (JStr "mkChar") [JStr $ show $ ord c]

exp2JS (CoreLit (CoreStr i)) = return $ JStr $ "\"" ++ showJstr i ++ "\""

exp2JS (CoreLet bs exp) = do
  eee <- exp2JS exp
  bbb <- mapM bnd2JS bs
  let letfun = JProc [] ((concat bbb) ++ [JReturn eee])
  return $ JCall letfun []


exp2JS app@(CoreApp f a) = do
  args <- mapM exp2JS a
  argsns <- mapM exp2JSns a
  fun  <- exp2JS f
  sat <- gets analSat
  sel <- gets analSel
  core <- gets coreRef
  strict <- gets analStrict
  forcedargs <- mapM evalForce args
  case f of
    CoreFun "SEQ" -> return $ JPrim "SEQ" args 
    CoreFun prim | isCorePrim (coreFunc core prim) -> return $ JPrim prim forcedargs
    CoreCon con  -> case con of
      "2" -> return $ JCall (JStr "new HSEOL") []
      "3" -> return $ JCall (JStr "new HSCons") argsns
      other -> return $ JCall (JStr "new HSData") [JStr con, JArray argsns]
    CoreFun  fn   -> case args of
      [] -> return fun
      _  -> do
        let selidx = sel app
            saturd = sat app
            strcts = take (length args) (strict fn ++ allFalse)
        if (selidx /= -1 && length a == 1)
          then return $ JIndex (JMember "_f" (head forcedargs)) (JStr $ show selidx)
          else do
            let wargs = zipWith ($) (map (\b -> if b then fst else snd) strcts) (zip args argsns)
            if saturd && all id strcts
              then return $ JCall (JMember "_b" (JStr fn)) wargs
              else return $ JMethod "_ap" fun [JArray wargs]
    _             -> return $ JMethod "_ap" fun [JArray argsns]


-- A special case for constructor matching where constructor is the only
-- in its algebraic datatype.

exp2JS (CoreCase exp [(PatCon (con@('(':'-':_)) sels, ce)]) = do
  jsexpf <- exp2JS exp >>= evalForce
  casenum <- getCnt
  flg <- gets optOpt
  coreref <- gets coreRef
  let casevar = JStr ("c" ++ show casenum)
      vdcl = JAssignV casevar jsexpf
      xsels = map CoreVar sels
  let caseapf ce = case sels of
        [] -> exp2JS ce
        _ | ce `elem` xsels -> return $ JIndex (JMember "_f" casevar) 
                                               (JStr $ show $ fromJust $ elemIndex ce xsels)
        _  -> do
          expce <- exp2JS ce
          let caseproc = JFunc sels expce
              conarity = findCtorArity coreref con
              mapply = JMethod "apply" caseproc [JStr "null", JMember "_f" casevar]
              conargs = JCall caseproc 
                              (map (JIndex (JMember "_f" casevar)) 
                                   (map (JStr . show . pred) [1 .. fromJust conarity]))
          return $ case conarity of
                     Nothing -> mapply
                     Just n | n <= oApplyLimit flg -> conargs
                     _ -> mapply
  capf <- caseapf ce
  let casebody = JReturn capf
      casefun = JProc [] [vdcl, casebody]
  
  return $ JCall casefun []

-- A general case

exp2JS (CoreCase exp alts) = do
  jsexp <- exp2JS exp
  jsexpf <- evalForce jsexp
  casenum <- getCnt
  flg <- gets optOpt
  coreref <- gets coreRef
  let casevar = JStr ("c" ++ show casenum)
      vdcl = JAssignV casevar jsexpf
      casevar' = case (head alts) of
        (PatCon _ _, _) -> JMember "_t" casevar
        other -> JMethod "valueOf" casevar []
      cases = map onecase alts
      ret ce = do
        cexp <- exp2JS ce
        return [JReturn cexp]
      onecase (PatLit (CoreInt i), ce) = do
        rc <- ret ce
        return (Just $ JNum (show i), rc, False)
      onecase (PatLit (CoreChr c), ce) = do
        rc <- ret ce
        return (Just $ JNum (show $ ord c), rc, False)
      onecase (PatDefault, ce) = do
        rc <- ret ce
        return (Nothing, rc, False)
      onecase (PatCon con sels, ce) = do
        capf <- caseapf ce
        return $ (Just $ JStr con, [JReturn capf], False) where
        xsels = map CoreVar sels
        caseapf ce = case sels of
          [] -> exp2JS ce
          _ | ce `elem` xsels -> return $ JIndex (JMember "_f" casevar) 
                                                 (JStr $ show $ fromJust $ elemIndex ce xsels)
          _  -> do
            expce <- exp2JS ce
            let caseproc = JFunc sels expce
                conarity = findCtorArity coreref con
                mapply = JMethod "apply" caseproc [JStr "null", JMember "_f" casevar]
                conargs = JCall caseproc 
                                (map (JIndex (JMember "_f" casevar)) 
                                     (map (JStr . show . pred) [1 .. fromJust conarity]))
            return $ case conarity of
                       Nothing -> mapply
                       Just n | n <= oApplyLimit flg -> conargs
                       _ -> mapply
      onecase z = error $ "onecase " ++ (show z)
  cases <- mapM onecase alts
  let swtch = JSwitch casevar' $ cases
      casefun = JProc [] [vdcl, 
                          swtch, 
                          JRaw ("throw('X (' + this._n + ') ' +" ++ 
                            strJexp casevar ++ ".toSource());\n")]
  return $ JCall casefun []

exp2JS _ = return $ JStr "zzzzzzzzz"

bnd2JS (str, exp) = func2JS 
  (CoreFunc {coreFuncName = str, coreFuncArgs = [], coreFuncBody = exp})

-- Find a constructor arity given its name. Since all applications of constructors
-- are saturated, find all applications of the given constructor and determine arity.
-- If by chance results are not consistent, return Nothing.

findCtorArity :: Core -> String -> Maybe Int

findCtorArity core str = 
  let ctorapps = filter (capp str) (allCore core)
      capp s (CoreApp (CoreCon c) _) = c == s
      capp _ _ = False
      carity (CoreApp (CoreCon c) exps) = length exps
      carity _ = 0
      arities = nub (map carity ctorapps)
  in case length arities of
    1 -> Just (head arities)
    _ -> Nothing



