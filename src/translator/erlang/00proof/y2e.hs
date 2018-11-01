-- A standalone ycr/yca converter to Erlang Core
--

module Main where

import System.Environment
import System.Directory
import System.FilePath
import System.Exit
import System.IO
import Control.Monad
import Control.Monad.State
import Yhc.Core
import Data.Char
import Data.List
import Data.Maybe
import Numeric
import GZip
import qualified Data.Map as M
import qualified Data.Set as S

import Bkeep
import Ebif
import Ecore
import Epretty

-- Stolen from Yhc's Main.hs: use the same base path as Yhc does.

getBasePath :: IO String
getBasePath = catch (getEnv "YHC_BASE_PATH") errHandle
    where
    errHandle e = do
        res <- getProgName >>= findExecutable
        case res of
            Nothing -> do
                hPutStrLn stderr $ 
                           "Warning: the environment variable YHC_BASE_PATH is not set\n" ++
                           "         and yhc cannot be found on the path"
                return ""
            Just x -> return $ takeDirectory $ takeDirectory x

fnforce = "_f_"
erlforce = ("hserl", "force")

dircon = ["Prelude;True", "Prelude;False", "Prelude;()"]

-- Create a custom strictness map for erlang primitives

erlstrct :: [CoreFunc] -> M.Map CoreFuncName [Bool]

erlstrct cfs = M.fromList erlprim where
  args p@(CorePrim {corePrimConv = conv}) | conv == "erlang" = 
    take (corePrimArity p) (repeat True)
  args _ = []
  ep = filter (not . null . args) cfs
  erlprim = zip (map coreFuncName ep) (map args ep)


-- Based on the customized strictness analysis function, insert
-- additional code to force function's arguments it is strict on.

strictify :: (Core -> (CoreFuncName -> [Bool])) -> Core -> Core

strictify sf core = core {coreFuncs = map f' (coreFuncs core)} where
  f' f@(CoreFunc {}) =
    let args = coreFuncArgs f
        args_f = map (++ "_f") args
        strct = sf core (coreFuncName f)
        argzf = zip3 strct args_f args
        arglet = concat $ map (\(a, b, c) -> if a 
          then [(c, CoreApp (CoreFun fnforce) [CoreVar b])]
          else []) argzf
        newargs = map (\(a, b, c) -> if a then b else c) argzf
        newbody = (if null arglet then id else CoreLet arglet) (coreFuncBody f)
    in f {coreFuncArgs = newargs, coreFuncBody = newbody}
  f' p@(CorePrim {}) = p

-- Map a variable name given the variable name map

mapVariable varmap s = let ns = M.lookup s varmap in fromMaybe s ns

-- Index names of variables given the variable name map

mapVarNames :: M.Map String String -> Core -> Core

mapVarNames varmap core = mapUnderCore (mapVarsInExpr varmap) core

-- Map variable names within an expression using the given map.

mapVarsInExpr :: M.Map String String -> CoreExpr -> CoreExpr

mapVarsInExpr vm (CoreVar s) = CoreVar (mapVariable vm s)
mapVarsInExpr vm (CoreLet bnds e) = 
  CoreLet (map (\(s, c) -> (mapVariable vm s, c)) bnds) e
mapVarsInExpr vm (CoreCase expr alts) = 
  CoreCase (mapVarsInExpr vm expr) (map (mapalt vm) alts) where
    mapalt vm (altpat, altexpr) = (mappat vm altpat, mapVarsInExpr vm altexpr) where
      mappat vm (PatCon s cvs) = PatCon s (map (mapVariable vm) cvs)
      mappat _ x = x
mapVarsInExpr vm (CoreApp fn args) = 
  let fn':args' = map (mapVarsInExpr vm) (fn:args)
  in  CoreApp fn' args'
mapVarsInExpr vm z = z


-- Force case variables before going into case expression. All occurrences
-- of the case variable inside the case expression will be replaced
-- with one holding the forced value.


forcecases :: Core -> Core

forcecases core = mapUnderCore onecase core where
  onecase (CoreCase (CoreVar cv) alts) =
    let cv' = cv ++ "_c"
        vmap = M.fromList [(cv, cv')]
        ncase = mapVarsInExpr vmap (CoreCase (CoreVar cv') alts)
    in  CoreLet [(cv', CoreApp (CoreFun fnforce) [CoreVar cv])] ncase
  onecase (CoreCase exp alts) = 
    let cv' = "e_" ++ show (calc_crc32 $ show exp) ++ "_c"
        ncase = CoreCase (CoreVar cv') alts
    in  CoreLet [(cv', CoreApp (CoreFun fnforce) [exp])] ncase
  onecase z = z


main = do
  args <- getArgs
  prog <- getProgName
  when (length args == 0) $ do 
    hPutStrLn stderr $ "usage: " ++ prog ++ "core [root root ...]"
    exitWith (ExitFailure 1)
  let (incore : roots0) = args ++ (map (\_ -> "") [1 .. ])
      roots = takeWhile (\x -> length x > 0) roots0
  when (null roots) $ do
    hPutStrLn stderr $ "warning: no root names specified"
  core0 <- loadCore incore
  rootsm <- if (null roots)
    then do let rr = coreName core0 ++ ";main"
            hPutStrLn stderr $ "assuming " ++ rr
            return [rr]
    else return roots
  hPutStrLn stderr $ "loaded core name = " ++ coreName core0
  let core = (coreReachable rootsm . 
              coreSimplify . 
              coreCaseElim . 
              removeRecursiveLet) core0
      funcs = filter ((/= "main") . coreFuncName) (coreFuncs core)
      custom = erlstrct funcs
      fnames = map coreFuncName funcs
      strfunc = coreStrictnessCustom custom
      fstrict = map (strfunc core) fnames
      fnstr = zip fnames fstrict
      core' = (forcecases . strictify strfunc) (core {coreFuncs = funcs})
  putStrLn "%% --------------  Strictness Map -------------------------------"
  mapM (putStrLn . ("%% " ++) . show) fnstr
  putStrLn "%% ----------------  Yhc Core  ----------------------------------"
  mapM (putStrLn . ("%% " ++)) $ lines $ show core'
  putStrLn "%% --------------  Functions Renamed  ---------------------------"
  let (emod, newst) = core2EM core' rootsm (M.fromList fnstr)
  mapM (putStrLn . ("%% " ++) . show) (M.assocs $ funcMap newst)
  putStrLn "%% --------------  Erlang Core  ---------------------------------"
  putStrLn (epMod emod)

-- Detect applications of selectors and determine index of a selector.

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

-- State monad for Erlang generator from Core.

data EG = EG {
   stateCnt :: Int                                -- counter to generate unique names
  ,currFun :: CoreFunc                            -- current function being compiled
  ,caseVars :: S.Set CoreVarName                  -- set of case variable names
  ,funcMap :: M.Map CoreFuncName CoreFuncName     -- map of function names (Yhc -> Erlang)
  ,strctMap :: M.Map CoreFuncName [Bool]          -- strictness map
  ,expFunc :: [CoreFuncName]                      -- list of exported functions
  ,coreRef :: Core                                -- reference to core
}               
                
getCnt :: EGM Int
                
getCnt = do     
  c <- gets stateCnt
  st <- get       
  put st {stateCnt = c + 1}
  return c
    
    
type EGM a = State EG a

-- Get Erlang module name from Core name.

erlModName :: Core -> String

erlModName core = "hs_" ++ map toLower (coreName core)

-- General function to return core function's arity

funcArity :: CoreFunc -> Int

funcArity f@(CorePrim {}) = corePrimArity f
funcArity f@(CoreFunc {}) = length $ coreFuncArgs f

-- Generate an Erlang module out of Yhc Core

core2EM :: Core -> [CoreFuncName] -> M.Map CoreFuncName [Bool] -> (EMod, EG)

core2EM core roots strct = 
  let env = EG {stateCnt = 1
               ,currFun = CoreFunc {coreFuncName = ""
                                   ,coreFuncArgs = []
                                   ,coreFuncBody = CoreLit (CoreInt 0)}
               ,funcMap = M.fromList [("Prelude;:", ".CONS"), ("Prelude;[]", ".EOL")]
               ,caseVars = S.empty
               ,strctMap = strct
               ,expFunc = roots
               ,coreRef = core}
  in  flip runState env $ do
        let notSkip (EFdef (EFname ("", _)) _ ) = False
            notSkip _ = True
            tocurry (f, a) = (f ++ "_c", 1)
        core <- gets coreRef
        efnms <- mapM func2Fname (coreFuncs core) >>= filterM (return . not . null . fst)
        efdefs1 <- mapM func2EFdef (coreFuncs core) >>= filterM (return . notSkip)
        efdefs2 <- mapM func2EFdefC (coreFuncs core) >>= filterM (return . notSkip)
        let emodnm = erlModName core
            efnmsc = map tocurry efnms
        return $ EMod emodnm (efnms ++ efnmsc) (efdefs1 ++ efdefs2)

-- Transform Haskell function name to conform with Erlang Core requirements.
-- Single quotes are replaced with dollar signs. Additionally, if function's
-- name is not on the exports list, it is replaced with somewhat shorter,
-- and the mapping is stored in the functions' map.

sq2d '\'' = '_'
sq2d '-' = '_'
sq2d ':' = '_'
sq2d z = z


transFname :: CoreFuncName -> EGM CoreFuncName

transFname cfn = do
  let cfn' = map sq2d cfn
  rr <- gets expFunc
  if (cfn `notElem` rr)
    then do
      m <- gets funcMap
      let cfn'' = fromMaybe "" $ M.lookup cfn m
      if null cfn''
        then do
          c <- getCnt
          let cft = dropModule cfn
              commas = if (length cft < 2) then "" else tail $ reverse $ tail cft
              tupcon = not (null commas) && all (== ',') commas
              tuplen = if tupcon then length commas else 0
              nn = "." ++ if tupcon then "TUP" ++ show tuplen else show c
              m' = M.insert cfn nn m
          st <- get
          put st {funcMap = m'}
          return nn
        else return cfn''
    else return $ dropModule cfn'

-- Convert function name to Erlang name/arity pair. Primitives and non-exported
-- functions (non-roots) will be skipped.

func2Fname :: CoreFunc -> EGM Fname

func2Fname cf@(CoreFunc {}) = do
  fn <- transFname (coreFuncName cf)
  return (fn, length $ coreFuncArgs cf)

func2Fname _ = return ("", 0)

-- Convert Yhc Core function definition to Erlang Core function definition.
-- Produce two interfaces: one, non-curried, for saturated calls only.
-- Another, curried, for partial applications and oversaturation.

func2EFdef :: CoreFunc -> EGM EFdef

func2EFdef cf@(CoreFunc {}) = do
  fn <- transFname (coreFuncName cf)
  st <- get
  put st {currFun = cf}
  fb <- expr2EX (coreFuncBody cf)
  let eargs = map (('_' :) . map sq2d) (coreFuncArgs cf)
      efn = (fn, funcArity cf)
  return $ EFdef (EFname efn) (EFun eargs fb)

func2EFdef _ = return $ EFdef (EFname ("", 0)) (EFun [] EXnil)

func2EFdefC :: CoreFunc -> EGM EFdef

func2EFdefC cf@(CoreFunc {}) | length (coreFuncArgs cf) > 1 = do
  fn <- transFname (coreFuncName cf)
  core <- gets coreRef
  let e:args = map (('_' :) . map sq2d) (coreFuncArgs cf)
      margs = map EXvar (e:args)
      fnc = fn ++ "_c"
      mkcurry [] = EXcall (EXatom $ erlModName core) (EXatom fn) margs
      mkcurry (a:as) = EXfun (EFun [a] $ mkcurry as)
  return $ EFdef (EFname (fnc, 1)) (EFun [e] $ mkcurry args)

func2EFdefC _ = return $ EFdef (EFname ("", 0)) (EFun [] EXnil)


-- Primitives map directly to BIFs/manually coded Erlang functions.

prim2bif :: CoreFunc -> Maybe BifSpec

prim2bif (CorePrim {coreFuncName = "MUL_W"}) = isBif "*"
prim2bif (CorePrim {coreFuncName = "ADD_W"}) = isBif "+"
prim2bif (CorePrim {coreFuncName = "SUB_W"}) = isBif "-"
prim2bif (CorePrim {coreFuncName = "LE_W" }) = isBif "<="
prim2bif (CorePrim {coreFuncName = "LT_W" }) = isBif "<"
prim2bif (CorePrim {coreFuncName = "GT_W" }) = isBif ">"
prim2bif (CorePrim {coreFuncName = "GE_W" }) = isBif ">="
prim2bif (CorePrim {coreFuncName = "EQ_W" }) = isBif "=="
prim2bif (CorePrim {coreFuncName = "NE_W" }) = isBif "=/="
prim2bif (CorePrim {coreFuncName = "NEG_W"  }) = Just ("erlang", "-", 1)
prim2bif (CorePrim {coreFuncName = "REM"  }) = Just ("erlang", "rem", 2)
prim2bif (CorePrim {coreFuncName = "QUOT" }) = Just ("erlang", "div", 2)
prim2bif (CorePrim {coreFuncName = "SEQ"  }) = Just ("hserl", "seq", 2)
prim2bif (CorePrim {coreFuncName = "YHC.Primitive;primThrow"}) = isBif "throw"
prim2bif (CorePrim {coreFuncName = "YHC.Primitive;primIntFromInteger"}) = 
  Just ("hserl", "identity", 1)
prim2bif (CorePrim {coreFuncName = "YHC.Primitive;primIntegerLt" }) = isBif "<"
prim2bif (CorePrim {coreFuncName = "YHC.Primitive;primIntegerSub" }) = isBif "-"
prim2bif (CorePrim {coreFuncName = "YHC.Primitive;primIntegerNeg"  }) = Just ("erlang", "-", 1)
prim2bif (CorePrim {coreFuncName = "YHC.Primitive;primIntegerEq" }) = isBif "=="

prim2bif (CorePrim {corePrimConv = "erlang", corePrimExternal = ext, corePrimArity = ar}) =
  let (emod, cefun) = span (/= ':') ext
      nomod = null emod
      nosep = null cefun
      efun  = tail cefun
  in  if nomod || nosep
        then Nothing
        else Just (emod, efun, ar)
  

prim2bif _ = Nothing

-- Convert a Yhc Core expression to Erlang Core expression.

expr2EX :: CoreExpr -> EGM EXpr

-- Position annotation (removed).

expr2EX (CorePos _ x) = expr2EX x

-- Literals.

expr2EX (CoreLit (CoreChr c)) = return $ EXnum $ fromIntegral $ ord c
    
expr2EX (CoreLit (CoreInt i)) = return $ EXnum $ fromIntegral i

expr2EX (CoreLit (CoreInteger j)) = return $ EXnum j

expr2EX (CoreLit (CoreFloat f)) = return $ EXfloat $ realToFrac f

expr2EX (CoreLit (CoreDouble d)) = return $ EXfloat d

expr2EX (CoreLit (CoreStr s)) = return $ EXtuple 
  [EXatom "@lst", erlList $ map (EXnum . fromIntegral . ord) s]

-- Applications.

-- Special case: force primitive application.

expr2EX (CoreApp (CoreFun fn) [arg]) | fn == fnforce = do
  marg <- expr2EX arg
  return $ EXforce marg

-- Application of an explicitly specified function.

expr2EX ap@(CoreApp ff@(CoreFun fn) args) = do
  core <- gets coreRef
  fex:margs <- mapM expr2EX (ff:args)
  strmap <- gets strctMap
  fmargs <- mapM selForce margs
  currf <- gets currFun
  let bools = fromMaybe [] $ M.lookup fn strmap
      saturd = coreSaturated core ap
  xfn <- transFname fn
  case coreFuncMaybe core fn of
    Nothing -> return $ EXunknown $ "unknown function " ++ fn ++ " applied"
    Just cf | isCorePrim cf ->
      case prim2bif cf of
        Just (m, f, a) -> return $ EXcall (EXatom m) (EXatom f) fmargs
        Nothing -> return $ 
          EXunknown $ "unknown primitive " ++ fn ++ "/" ++ corePrimExternal cf ++ " applied"
    Just cf | isCoreFunc cf && null args ->
      return fex
    Just cf | isCoreFunc cf && null (coreFuncArgs cf) ->
      
      return $ EXthunk fex 1 margs
    Just cf | isCoreFunc cf && saturd ->
      return $ EXthunk (EXtuple [EXatom $ erlModName core, EXatom xfn])
                       (fromIntegral $ funcArity cf)
                       margs
    Just cf | isCoreFunc cf -> do
      let xfnc = if length (coreFuncArgs cf) <= 1 then xfn else xfn ++ "_c"
      return $ EXthunk (EXtuple [EXatom $ erlModName core, EXatom xfnc]) 1 margs
    _ -> return $ EXunknown $ showRaw ap

-- Application of an expression that evaluates to a function.  

expr2EX (CoreApp xv@(CoreVar v) args) = do
  mxv:margs <- mapM expr2EX (xv:args)
  return $ EXthunk mxv 1 margs

-- Application of a constructor (expected to be saturated).

expr2EX (CoreApp (CoreCon tfcn) []) | tfcn `elem` dircon = do
  let at = map toLower $ dropModule tfcn
  return $ EXatom at

expr2EX ap@(CoreApp (CoreCon cn) args) = do
  core <- gets coreRef
  margs <- mapM expr2EX args
  when (not $ coreSaturated core ap) $ error $ "non-saturated application of constructor " ++ cn
  xcn <- transFname cn
  return $ EXtuple $ [EXatom "@dt", EXatom xcn] ++ margs

-- Case statements.

expr2EX cs@(CoreCase (CoreVar cv) ptex) = do
  cvset <- gets caseVars
  st <- get
  put st {caseVars = cv `S.insert` cvset}
  epats <- mapM pat2EClause ptex
  return $ EXcase ['_':(map sq2d cv)] epats

-- Just a variable, resolves to its name prefixed with underscore.

expr2EX (CoreVar v) = return $ EXvar ('_':(map sq2d v))

expr2EX (CoreFun f) = do
  core <- gets coreRef
  xfn <- transFname f
  case coreFuncMaybe core f of
    Nothing -> return $ EXunknown $ "unknown function " ++ f ++ " passed as value"
    Just cf | isCoreFunc cf && length (coreFuncArgs cf) > 1 ->
      return $ EXtuple [EXatom $ erlModName core, EXatom (xfn ++ "_c")]
    Just cf | isCoreFunc cf && length (coreFuncArgs cf) == 0 ->
      return $ EXcaf (erlModName core) xfn
    Just cf | isCorePrim cf ->
      case prim2bif cf of
        Nothing -> return $ EXunknown $ "unknown primitive " ++ f ++ " passed as value"
        Just (m, f, a) -> forcePrimArgs m f a 
    Just cf ->
      return $ EXtuple [EXatom $ erlModName core, EXatom xfn]

-- Let expression (not assumed to be a letrec).

expr2EX (CoreLet ls e) = do
  let vs = map (('_':) . (map sq2d) . fst) ls
  es <- mapM (expr2EX . snd) ls
  ee <- expr2EX e
  return $ EXlet vs es ee

expr2EX z = return (EXunknown $ showRaw z)

-- Convert a Yhc Core pattern to Erlang Core clause

pat2EClause (PatLit (CoreInt i), ce) = do
  ece <- expr2EX ce
  return $ EClause [EPnum $ fromIntegral i] [EXatom "true"] [ece]

pat2EClause (PatCon tfcn [], ce) | tfcn `elem` dircon = do
  ece <- expr2EX ce
  let at = map toLower $ dropModule tfcn
  return $ EClause [EPatom at] [EXatom "true"] [ece]

pat2EClause (PatCon cn dvs, ce) = do
  ece <- expr2EX ce
  xcn <- transFname cn
  return $ EClause [EPtuple $ [EPatom "@dt", EPatom xcn] ++ map (EPvar . ('_':)) dvs]
                   [EXatom "true"] [ece]

pat2EClause (PatDefault, ce) = do
  ece <- expr2EX ce
  return $ EClause [EPdcare] [EXatom "true"] [ece]
  
-- Selectively force an expression.

selForce :: EXpr -> EGM EXpr

-- If expression to force is a case variable, don't force it.
-- Nor do force it if it is all of function's strict-on arguments.

selForce z@(EXvar ('_':v)) = do
  cvset <- gets caseVars
  currf <- gets currFun
  let cfargs = coreFuncArgs currf
  if v `S.member` cvset || (v ++ "_f") `elem` cfargs
    then return z
    else return (EXforce z)

selForce z@(EXnum _) = return z

selForce z@(EXfloat _) = return z

selForce z = return (EXforce z)

-- Generate a lambda around an Erlang primitive which forces all of its arguments.

forcePrimArgs :: String -> String -> Arity -> EGM EXpr

forcePrimArgs mod fun 0 = do
  return $ EXcall (EXatom mod) (EXatom fun) []

forcePrimArgs mod fun ar = do
  nmbs <- replicateM ar getCnt
  let f:rmvars = map (("_ll" ++) . show) nmbs
      fcdvars = map (EXforce . EXvar) (f:rmvars)
      mkcurry [] = EXcall (EXatom mod) (EXatom fun) fcdvars
      mkcurry (a:as) = EXfun (EFun [a] $ mkcurry as)
  return $ EXfun (EFun [f] $ mkcurry rmvars)


