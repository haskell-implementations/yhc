-- Prepare Core to Javascript generation

module JS.CorePrep where

import JS.CoreJS (funcEliminable)
import JS.OptOpt
import System.IO
import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Map as M
import Yhc.Core
import Numeric
import Control.Monad
import Control.Monad.State

-- Show a number using the set of 62 characters: 26 lowercase letters,
-- 26 uppercase letters, and 10 digits.

show62 :: (Integral a, Show a) => a -> ShowS

show62 n = showIntAtBase 62 ic62 n
  where ic62 n | n >= 0  && n < 26 = chr (ord 'a' + n)
               | n >= 26 && n < 52 = chr (ord 'A' + n - 26)
               | n >= 52 && n < 62 = chr (ord '0' + n - 52)
               | otherwise = '?'

-- Utility functions to handle soecial assignments for constructor
-- and other indices.

swapTuple (a, b) = (b, a)

substIdx (_, s) | s == "Prelude;False" = (s, "false")
substIdx (_, s) | s == "Prelude;True"  = (s, "true")
substIdx (_, s) | s == "Prelude;[]"    = (s, "2")
substIdx (_, s) | s == "Prelude;:"     = (s, "3")
substIdx z                             = swapTuple z

-- Format a Javascript index entry

idxEntry f v (s, n) = v ++ "[" ++ f s ++ "] = " ++ n ++ ";"

-- Write a map as a Javascript index

writeMapIdx :: Handle -> (String -> String) -> (M.Map String String) -> String -> IO ()

writeMapIdx h f m i = 
  mapM_ (hPutStrLn h . idxEntry f i) (M.assocs m)

-- Build a map of constructors that will be used for indexing

buildConMap :: Core -> M.Map String String

buildConMap core = M.fromList ac where
  allcore = allCore core
  case2con (CoreCase _ pxps) = concatMap (pat2expr . fst) pxps where
    pat2expr (PatCon s _) = [CoreCon s]
    pat2expr _ = []
  case2con _ = []
  ac = (map (negIdx core) .
        map substIdx .
        zip (map show [4..]) .
        map head .
        group .
        sort .
        map conname) $
        ((filter constr allcore) ++ (concatMap case2con allcore))
  conname (CoreCon s) = s
  constr (CoreCon _) = True
  constr _           = False

-- Negate indices of constructors that are single in their datatypes

negIdx :: Core -> (String, String) -> (String, String)

negIdx core (con, idx) = (con, mm ++ idx ++ nn) where
  datas = filter (hascon con) (coreDatas core)
  hascon con adt = (filter (fldcon con) (coreDataCtors adt) /= [])
  fldcon con cc = (coreCtorName cc == con)
  (mm, nn) = case length datas of
    1 -> case length (coreDataCtors (head datas)) of
           1 -> ("(-", ")")
           _ -> ("", "")
    _ -> ("", "")

-- Build a map of functions that will be used for indexing

buildFunMap :: Core -> M.Map String String

buildFunMap core = M.fromList af where
  af = (map swapTuple .
        zip (map (("F_"++) . flip show62 "") [1..]) .
        map head . 
        group .
        sort . 
        filter (not . isSuffixOf ";unsafeJS") .
        map coreFuncName) (filter isCoreFunc $ coreFuncs core)

-- Build a map of variables that will be used for indexing

buildVarMap :: Core -> M.Map String String

buildVarMap core = M.fromList av where
  av = (map swapTuple .
        zip (map (("_" ++) . flip show62 "") [1..]) .
        map head .
        group .
        sort .
        filter (/= "_") .
        map varname .
        filter variab) ((allCore core) ++ (concat $ map letvars (allCore core)))
  variab (CoreVar _) = True
  variab _           = False
  varname (CoreVar s) = s
  letvars (CoreLet bnds _) = map (CoreVar . fst) bnds
  letvars _ = []

-- Build a map of functions that will be eliminated during optimization

buildEliMap :: Core -> JSOptFlags -> M.Map String String

buildEliMap core flg = M.fromList ef where
  ef = (map swapTuple .
        zip (map show [0, 0 ..]) .
        filter (not . isSuffixOf "unsafeJS") .
        map coreFuncName) (filter (funcEliminable flg) (coreFuncs core))

-- Index function definitions given the function name map
-- Names of primitives are not affected

indexFunDefs :: M.Map String String -> M.Map String String -> Core -> Core

indexFunDefs funmap varmap core = core {coreFuncs = map onefun (coreFuncs core)} where
  onefun cf | isCoreFunc cf =
                  cf {coreFuncName = let ns = M.lookup (coreFuncName cf) funmap in
                  fromMaybe (coreFuncName cf) ns,
                  coreFuncArgs = map (mapVariable varmap) (coreFuncArgList cf)}
  onefun cf = cf 


-- Map a variable name given the variable name map

mapVariable varmap s = let ns = M.lookup s varmap in fromMaybe s ns

-- Index names of variables given the variable name map

mapVarNames :: M.Map String String -> Core -> Core

mapVarNames varmap core = mapUnderCore mapvar core where
  mapvar (CoreVar s) = CoreVar (mapVariable varmap s)
  mapvar (CoreLet bnds e) = CoreLet (map (\(s, c) -> (mapVariable varmap s, c)) bnds) e
  mapvar (CoreCase expr alts) = CoreCase (mapvar expr) (map (mapalt varmap) alts) where 
    mapalt varmap (altpat, altexpr) = (mappat varmap altpat, mapvar altexpr) where
      mappat varmap (PatCon s cvs) = PatCon s (map (mapVariable varmap) cvs)
      mappat _ x = x
  mapvar z = z

-- Index names of functions given the function name map

mapFunNames :: M.Map String String -> Core -> Core

mapFunNames funmap core = mapUnderCore mapfun core where
  mapfun (CoreFun s) = let ns = M.lookup s funmap in CoreFun $ fromMaybe s ns
  mapfun z = z

-- Index names of constructors given the constructor name map

mapConNames :: M.Map String String -> Core -> Core

mapConNames conmap core = mapUnderCore mapcon core where
  mapcon (CoreCon s) = let ns = M.lookup s conmap in CoreCon $ fromMaybe s ns
  mapcon (CoreCase expr alts) = CoreCase (mapcon expr) (map (mapalt conmap) alts) where 
    mapalt conmap (altpat, altexpr) = (mappat conmap altpat, mapcon altexpr) where
      mappat conmap (PatCon s cvs) = 
        let ns = M.lookup s conmap in PatCon (fromMaybe s ns) cvs
      mappat _ x = x
  mapcon z = z

-- Create aliases of functions with different arity to have more saturated calls.

mkArityAlias :: Core -> Core

mkArityAlias core = Core {coreFuncs = cf, 
                          coreName = "", 
                          coreImports = [],
                          coreDatas = []} where
  unsfjs0 = [x | x@(CoreFunc nam _ _) <- coreFuncs core, dropModule nam == "unsafeJS"]
  unsfjs = case unsfjs0 of
    [] -> error "unsafeJS is not defined"
    ns -> coreFuncName $ head ns
  cf = concat $ map mkaa (coreFuncs core)
  arity = length . coreFuncArgList
  aa f n = let sa = take n (coreFuncArgList f)
               fn = coreFuncName f
               ujsfn = '#' : drop 1 (aliasName fn n)
               ubody = CoreApp (CoreFun unsfjs) [CoreApp (CoreFun ujsfn) []]
               als = CoreFunc {coreFuncName = aliasName fn n,
                               coreFuncArgs = sa,
                               coreFuncBody = ubody}
               ujs = CoreFunc {coreFuncName = ujsfn,
                               coreFuncArgs = [],
                               coreFuncBody = CorePos "###" (CoreLit (CoreStr "@@@"))}
           in  [als, ujs]
  mkaa f | arity f <= 1 = []
  mkaa f = concat $ map (aa f) [1 .. (arity f - 1)]

fixArityAlias :: M.Map String String -> Core -> Core

fixArityAlias funmap core = core {coreFuncs = fxf} where
  fxf = map fxa (coreFuncs core)
  fxa cf | coreFuncArgList cf == [] && 
           coreFuncBody cf == CorePos "###" (CoreLit (CoreStr "@@@")) = 
    case coreFuncName cf of
      '#':targfn ->
        let (ar, _:targn) = span (/= '@') targfn
            az = map (: []) ['a' .. 'z']
            saa = take arn az
            arn = read ar
            mbfn = M.lookup targn funmap
            ubstr f sa = "return new HSDly(" ++ f ++ "," ++ f ++ ",[" ++
                      concat (intersperse "," sa) ++ "])"
        in case mbfn of
             Nothing -> cf
             Just fn -> cf {coreFuncBody = CorePos "%%%" (CoreLit (CoreStr $ ubstr fn saa))}
      _ -> cf
  fxa z = z


-- If there is a call to a function with N arguments, try to find its arity alias
-- call to which would be saturated.

useArityAlias :: Core -> Core  -> Core

useArityAlias core ca = mapUnderCore uaa core where
  uaa a@(CoreApp (CoreFun ('@':_)) args) = a
  uaa a@(CoreApp (CoreFun f) args) =
    let nargs = length args
        fsuff = aliasName f nargs
    in  case coreFuncMaybe ca fsuff of
        Nothing -> a
        Just cf -> CoreApp (CoreFun fsuff) args
  uaa z = z

aliasName f n = "@" ++ show n ++ "@" ++ f

mergeArityAlias :: Core -> Core -> Core

mergeArityAlias core ca = core {coreFuncs = coreFuncs core ++ coreFuncs ca}

arityAlias core =
  let ca = mkArityAlias core
      core' = useArityAlias core ca
  in  mergeArityAlias core' ca

