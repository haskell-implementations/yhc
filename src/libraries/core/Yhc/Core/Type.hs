{-# OPTIONS_DERIVE --module=Yhc.Core.Binary --derive=BinaryOld --output=Binary.hs #-}
{-# OPTIONS_DERIVE --import --import=Yhc.Core.Internal.Binary --import=Control.Monad #-}

module Yhc.Core.Type where

-- while it may seem tempting to add type signatures to Core
-- it won't work - by this stage all the type signatures are
-- wrong because of desugarring

import Control.Monad(liftM)
import Data.Maybe(fromMaybe, listToMaybe, mapMaybe)
import Data.Char(isSpace)
import Data.List(intersperse)
import qualified Data.Map as Map


{-! global: GhcBinary !-}

type CoreVarName = String
type CoreFuncName = String
type CoreDataName = String
type CoreCtorName = String
type CoreFieldName = String

-- module name, imports, items in the module
data Core = Core {coreName :: String, coreImports :: [String],
                  coreDatas :: [CoreData], coreFuncs :: [CoreFunc]}
            deriving (Eq,Ord)

data CoreData = CoreData {coreDataName :: CoreDataName, coreDataTypes :: [String], coreDataCtors :: [CoreCtor]}
                deriving (Eq,Ord)

-- Name, then list of maybe field names
data CoreCtor = CoreCtor {coreCtorName :: CoreCtorName, coreCtorFields :: [(String, Maybe CoreFieldName)]}
                deriving (Eq,Ord)

data CoreFunc = CoreFunc {coreFuncName :: CoreFuncName, coreFuncArgs :: [CoreVarName], coreFuncBody :: CoreExpr}
              | CorePrim {
                    coreFuncName :: CoreFuncName,
                    corePrimArity :: Int,
                    corePrimExternal :: String,
                    corePrimConv :: String,
                    corePrimImport :: Bool,
                    corePrimTypes :: [String]
                }
                deriving (Eq,Ord)

isCoreFunc, isCorePrim :: CoreFunc -> Bool
isCoreFunc (CoreFunc{}) = True; isCoreFunc _ = False
isCorePrim (CorePrim{}) = True; isCorePrim _ = False


coreFuncArity :: CoreFunc -> Int
coreFuncArity (CorePrim{corePrimArity=x}) = x
coreFuncArity x = length $ coreFuncArgs x

-- An universal replacement for coreFuncArgs that now does not match in all cases

coreFuncArgList :: CoreFunc -> [CoreVarName]
coreFuncArgList (CorePrim{coreFuncName=n,corePrimArity=x}) = take x $ map (("__" ++ n ++ "_") ++) (map show [1..])
coreFuncArgList x = coreFuncArgs x

type CoreFuncMap = Map.Map CoreFuncName CoreFunc



data CoreExpr = CoreCon CoreCtorName
              | CoreVar CoreVarName
              | CoreFun CoreFuncName
              | CoreApp CoreExpr [CoreExpr]
              | CoreLam [CoreVarName] CoreExpr
              | CoreCase CoreExpr [(CorePat,CoreExpr)]
              | CoreLet [(CoreVarName,CoreExpr)] CoreExpr
              | CorePos String CoreExpr
              | CoreLit CoreLit
                deriving (Ord,Eq)


data CoreLit = CoreInt Int
             | CoreInteger Integer
             | CoreChr Char
             | CoreStr String
             | CoreFloat Float
             | CoreDouble Double
               deriving (Ord,Eq,Show)


data CorePat = PatCon {patCon :: CoreCtorName, patVars :: [CoreVarName]}
             | PatLit {patLit :: CoreLit}
             | PatDefault
               deriving (Ord,Eq,Show)


-- smart constructors
coreApp :: CoreExpr -> [CoreExpr] -> CoreExpr
coreApp x [] = x
coreApp x xs = CoreApp x xs

coreLet :: [(CoreVarName,CoreExpr)] -> CoreExpr -> CoreExpr
coreLet [] x = x
coreLet xs x = CoreLet xs x

coreLam :: [CoreVarName] -> CoreExpr -> CoreExpr
coreLam [] x = x
coreLam xs x = CoreLam xs x


fromCoreLit :: CoreExpr -> CoreLit
fromCoreLit (CoreLit x) = x
fromCoreLit x = error $ "Yhc.Core.fromCoreLit on a non-literal"

fromCoreCon, fromCoreVar, fromCoreFun :: CoreExpr -> String
fromCoreCon  (CoreCon  x) = x
fromCoreVar  (CoreVar  x) = x
fromCoreFun  (CoreFun  x) = x

fromCoreApp :: CoreExpr -> (CoreExpr,[CoreExpr])
fromCoreApp (CoreApp x y) = (x,y)
fromCoreApp x = (x,[])

fromCoreLet :: CoreExpr -> ([(CoreVarName,CoreExpr)],CoreExpr)
fromCoreLet (CoreLet x y) = (x,y)
fromCoreLet x = ([],x)

fromCoreLam :: CoreExpr -> ([CoreVarName],CoreExpr)
fromCoreLam (CoreLam x y) = (x,y)
fromCoreLam x = ([],x)

isCoreCon, isCoreVar, isCoreFun, isCoreLam :: CoreExpr -> Bool
isCorePos, isCoreLet, isCoreCase, isCoreLit :: CoreExpr -> Bool
isCoreCon  x = case x of {CoreCon{}  -> True; _ -> False}
isCoreVar  x = case x of {CoreVar{}  -> True; _ -> False}
isCoreFun  x = case x of {CoreFun{}  -> True; _ -> False}
isCoreLam  x = case x of {CoreLam{}  -> True; _ -> False}
isCorePos  x = case x of {CorePos{}  -> True; _ -> False}
isCoreLet  x = case x of {CoreLet{}  -> True; _ -> False}
isCoreCase x = case x of {CoreCase{} -> True; _ -> False}
isCoreLit  x = case x of {CoreLit{}  -> True; _ -> False}

isCoreStr, isCoreChr, isCoreInt :: CoreLit -> Bool
isCoreStr  x = case x of {CoreStr{}  -> True; _ -> False}
isCoreChr  x = case x of {CoreChr{}  -> True; _ -> False}
isCoreInt  x = case x of {CoreInt{}  -> True; _ -> False}

isPatDefault, isPatLit, isPatCon :: CorePat -> Bool
isPatDefault x = case x of {PatDefault{} -> True; _ -> False}
isPatLit     x = case x of {PatLit{}     -> True; _ -> False}
isPatCon     x = case x of {PatCon{}     -> True; _ -> False}


{-# DEPRECATED fromPatLit "use patLit instead" #-}
fromPatLit = patLit

patVariables (PatCon _ xs) = xs
patVariables _ = []

patToExpr :: CorePat -> CoreExpr
patToExpr (PatCon c xs) = coreApp (CoreCon c) (map CoreVar xs)
patToExpr (PatLit x) = CoreLit x
patToExpr PatDefault = CoreVar "_"

exprToPat :: CoreExpr -> CorePat
exprToPat (CoreApp (CoreCon c) vs) = PatCon c (map fromCoreVar vs)
exprToPat (CoreCon c) = PatCon c []
exprToPat (CoreLit x) = PatLit x
exprToPat (CoreVar _) = PatDefault


-- | Returns true for constants that take a small, bounded
-- amount of space
isCoreLitSmall :: CoreLit -> Bool
isCoreLitSmall x = isCoreInt x || isCoreChr x


remCorePos :: CoreExpr -> CoreExpr
remCorePos (CorePos _ x) = x
remCorePos x = x


-- | drop a module from a Core declaration
dropModule :: String -> String
dropModule x = f x False x
    where
        f x False (';':_) = x
        f _ True  (';':x) = f x False x
        f x _ (_:xs) = f x True xs
        f x _ [] = x


-- | Get a function from a Core type
--   crashes if the function does not exist
coreFunc :: Core -> CoreFuncName -> CoreFunc
coreFunc core name = fromMaybe (error msg) (coreFuncMaybe core name)
    where msg = "Yhc.Core.Type.coreFunc, function not found: " ++ name


-- | A non-crashing version of 'coreFunc'
--   returns Nothing if the function does not exist.
--   If multiple functions with the same name exist, this crashes.
coreFuncMaybe :: Core -> CoreFuncName -> Maybe CoreFunc
coreFuncMaybe core name =
    case [x | x <- coreFuncs core, coreFuncName x == name] of
        [] -> Nothing
        [x] -> Just x
        xs -> error $ "Yhc.Core.Type.mbCoreFunc, found found " ++ show (length xs) ++ " times: " ++ name



-- | Get a 'CoreData' from a field (the snd element of 'coreCtorFields')
coreFieldDataMaybe :: Core -> CoreFieldName -> Maybe CoreData
coreFieldDataMaybe core name = coreFieldCtorMaybe core name >>= coreCtorDataMaybe core . coreCtorName

-- | Get a 'CoreData' from a ctor name
coreCtorDataMaybe :: Core -> CoreCtorName -> Maybe CoreData
coreCtorDataMaybe core name = listToMaybe [dat | dat <- coreDatas core, name `elem` map coreCtorName (coreDataCtors dat)]

-- | Get a 'CoreCtor' from a field name
coreFieldCtorMaybe :: Core -> CoreFieldName -> Maybe CoreCtor
coreFieldCtorMaybe core name = listToMaybe [ctr | dat <- coreDatas core, ctr <- coreDataCtors dat
                                           , name `elem` mapMaybe snd (coreCtorFields ctr)]


coreFieldData :: Core -> CoreFieldName -> CoreData
coreFieldData core name = fromMaybe (error msg) $ coreFieldDataMaybe core name
    where msg = "Yhc.Core.coreFieldData, looking for " ++ name

coreCtorData :: Core -> CoreCtorName -> CoreData
coreCtorData core = fromMaybe (error "Yhc.Core.coreCtorData") . coreCtorDataMaybe core

coreFieldCtor :: Core -> CoreFieldName -> CoreCtor
coreFieldCtor core = fromMaybe (error "Yhc.Core.coreFieldCtor") . coreFieldCtorMaybe core

coreCtor :: Core -> CoreCtorName -> CoreCtor
coreCtor core name = head [ctr | dat <- coreDatas core, ctr <- coreDataCtors dat, coreCtorName ctr == name]

coreData :: Core -> CoreDataName -> CoreData
coreData core name = head [dat | dat <- coreDatas core, coreDataName dat == name]


-- | Take a function that operates on bodies, and apply it to a program
applyBodyCore :: (CoreExpr -> CoreExpr) -> (Core -> Core)
applyBodyCore f = applyFuncCore (applyBodyFunc f)


-- | Take a function that operates on bodies, and apply it to a function
applyBodyFunc :: (CoreExpr -> CoreExpr) -> (CoreFunc -> CoreFunc)
applyBodyFunc f func | isCoreFunc func = func{coreFuncBody = f (coreFuncBody func)}
                     | otherwise = func


-- | Take a function that operates on functions, and apply it to a program
applyFuncCore :: (CoreFunc -> CoreFunc) -> (Core -> Core)
applyFuncCore f core = core{coreFuncs = map f (coreFuncs core)}


applyCtorCore :: (CoreCtor -> CoreCtor) -> (Core -> Core)
applyCtorCore f = applyDataCore (applyCtorData f)

applyDataCore :: (CoreData -> CoreData) -> (Core -> Core)
applyDataCore f core = core{coreDatas = map f (coreDatas core)}

applyCtorData :: (CoreCtor -> CoreCtor) -> (CoreData -> CoreData)
applyCtorData f dat = dat{coreDataCtors = map f (coreDataCtors dat)}


applyBodyCoreM :: Monad m => (CoreExpr -> m CoreExpr) -> Core -> m Core
applyBodyCoreM f = applyFuncCoreM g
    where
        g (CoreFunc a b c) = liftM (CoreFunc a b) $ f c
        g x = return x


applyFuncCoreM :: Monad m => (CoreFunc -> m CoreFunc) -> Core -> m Core
applyFuncCoreM f c = do
    res <- mapM f (coreFuncs c)
    return $ c{coreFuncs = res}



-- | Split up a coreDataType into lexical elements
--   None of the result elements will be space, or blank
--   Some may be "(", ")" or "!"
coreDataTypeSplit :: String -> [String]
coreDataTypeSplit [] = []
coreDataTypeSplit (x:xs)
        | x `elem` special = [x] : coreDataTypeSplit xs
        | isSpace x = coreDataTypeSplit xs
        | otherwise = let (a,b) = break (\x -> isSpace x || x `elem` special) (x:xs)
                      in a : coreDataTypeSplit b
    where
        special = "!()"


-- | can pretty print much nicer, just something that works for now
coreDataTypeJoin :: [String] -> String
coreDataTypeJoin = concat . intersperse " "



fromCoreFuncMap :: Core -> CoreFuncMap -> Core
fromCoreFuncMap core fm = core{coreFuncs = Map.elems fm}

toCoreFuncMap :: Core -> CoreFuncMap
toCoreFuncMap core = Map.fromList [(coreFuncName x, x) | x <- coreFuncs core]

coreFuncMap :: CoreFuncMap -> CoreFuncName -> CoreFunc
coreFuncMap fm name = fromMaybe (error $ "Yhc.Core.coreFuncMap, function not found, " ++ name) $
                      Map.lookup name fm

coreFuncMapMaybe :: CoreFuncMap -> CoreFuncName -> Maybe CoreFunc
coreFuncMapMaybe fm name = Map.lookup name fm


