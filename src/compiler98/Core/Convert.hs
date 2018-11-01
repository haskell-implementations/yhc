module Core.Convert(makeCore) where

import Id
import Util.Extra(mixLine,mixSpace,mix)
import PosCode
import StrPos
import List
import Char
import Util.Extra
import Error
import IntState
import Maybe
import NT
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import Yhc.Core

-- | internal compiler state
data CState = CState {
                csState :: IntState,
                csBinds :: [(Id,String)],
                csFree :: Set.Set Id,
                csFails :: [CoreExpr],
                csNextFail :: Int
              }

-- | compiler monad
type CMonad a = State CState a

-- | convert pos lambda to yhc core
makeCore :: [String] -> IntState -> [Id] -> [(Id,PosLambda)] -> (Core,[Id,String])
makeCore imports state datas funs = (core,csBinds cstate)
    where
    modu          = getModuleId state
    (core,cstate) = runState cProgram (CState state Map.empty Set.empty [] 0)

-- | convert a program to a core program
cProgram :: [String] -> [Id] -> [(Id,PosLambda)] -> CMonad Core
cProgram imports datas funcs = do
    datas' <- mapM cData datas
    funcs' <- mapM cFunc funcs
    state  <- getState
    return $ Core (getModuleId state) imports datas' funcs'

-- | convert a data to a core data
cData :: Id -> CMonad CoreData
cData i = do
    ctors <- mapM cCtor childs
    return $ CoreData name' (map strTVar free) childs
    where
        name'              = fixTuple $ show name
        NewType free _ _ _ = typ
        (InfoData _ name _ typ children) = fst $ getInfo i () state
        childs = case children of
                    (DataNewType _ x) -> x
                    (Data _ x) -> x

-- | convert a constructor to core
cCtor :: Id -> CMonad CoreCtor
cCtor i = do
    bind i name'
    state <- getState
    return $ CoreCtor name' $ zip (map cType targs) (map cField fields)
    where
        name'               = (fixTuple $ show name)
        NewType _ _ _ targs = typ
        (InfoConstr _ name _ _ typ fields _) = fst $ getInfo i () state

        cField Nothing = Nothing
        cField (Just x) = Just $ dropModule $ strIS state x

        cType x = strNT (getName state) strTVar x

-- | convert a function to core
cFunc :: (Id,PosLambda) -> CMonad CoreFunc
cFunc (i, PosLambda pos int fvs bvs e) = do
        name <- bindRealName i
        e' <- cExpr e
        args <- mapM_ (bindName . snd) bvs
        return $ CoreFunc name args (wrapPos pos e')
    where
    wrapPos pos x | pos == noPos = x
                  | otherwise    = CorePos (show pos) x

cFunc (i, item) = do
    state <- getState
    name <- bindRealName i
    return $ CorePrim name (arityIS state i)

-- | convert an expression to core
cExpr :: PosExp -> CoreExp
cExpr x = case x of
    -- literals
    PosInt _ i -> return $ CoreInt i
    PosInteger _ i -> return $ CoreInteger i
    PosChar _ c -> return $ CoreChr c
    PosString _ s -> return $ CoreStr s
    PosFloat _ f -> return $ CoreFloat f
    PosDouble _ d -> return $ CoreDouble d

    -- simple expressions
    PosExpDict e -> cExpr e
    PosExpThunk p _ args -> cExpr (PosExpApp p args)
    PosCon _ i -> bindCon i
    PosPrim _ prim _ -> return $ CoreFun $ strPrim prim
    PosVar _ i -> do
        nam <- bindRealName i
        free <- isFree i
        return $ if free then CoreVar nam else CoreFun nam

    PosExpApp _ (a:as) = do
        a' <- cExpr a
        if a == CoreFun "STRING" then cExpr (head as)
         else do
            as' <- mapM cExpr as
            return (CoreApp a' as')

    -- let bindings
    PosExpLet _ _ [] e -> cExpr e
    PosExpLet _ _ bs e -> inNewEnv $ do
        mapM_ (addFree . snd) bs
        ns <- mapM_ (bindRealName . snd) bs
        binds <- zipWithM (\(i,PosLambda _ _ e) n -> do { x <- cExpr e ; return (x,n) }) bs ns
        e' <- cExpr e
        return (CoreLet binds e')

    -- If and Case
    PosExpIf pos g e1 e2 e3 -> do
        e1' <- cExpr e1
        e2' <- cExpr e2
        e3' <- cExpr e3
        let true = CoreApp (CoreCon "Prelude.True") []
            false = CoreApp (CoreCon "Prelude.False") []
        return $ CoreCase e1' [(true, e2'),(false,e3')]

    PosExpCase pos e alts -> do
        e' <- cExpr e
        alts' <- mapM_ cAlt alts
        return $ CoreCase e' alts
        where
        cAlt (PosAltInt pos i False e) = do { x <- cExpr e ; return (CoreChr (chr i), x) }
        cAlt (PosAltInt pos i True e) = do { x <- cExpr e ; return (CoreInt i, x) }
        cAlt (PosAltCon pos c vars e) = inNewEnv $ do
            mapM_ (addFree . snd) vars
            vs <- mapM (bindName . snd) vars
            con <- bindCon c
            e' <- cExpr e
            return $ CoreApp con (map CoreVar vs) e'

    -- fat bar and fail
    PosExpFatBar _ e1@(PosExpCase {}) PosExpFail -> do
        CoreCase a b <- cExpr e1
        return $ CoreCase a (b ++ [(CoreVar "_",failExpr)])

    PosExpFarBar _ e1 PosExpFail -> cExpr e1
    PosExpFarBar pos e1 e2 -> do
        e2' <- cExpr e2
        inNewFailure (\v -> CoreVar $ "v_fail_"++show v) $ \ var -> do
            e1' <- cExpr (PosExpFatBar pos e1 PosExpFail)
            return $ CoreLet [(var, e2')] e1'

    PosExpFail -> getFailExpr
    other -> do
        state <- getState
        raiseError $ ErrorInternal "Core.Core.cExpr" (strPExpr (strIS state) "" other)

-- | perform computation inside a new environment
inNewEnv :: CMonad a -> CMonad a
inNewEnv f = do
    cs <- get
    x <- f
    modify $ \ cs' -> cs' { csFree = csFree cs }
    return x

-- | perform computation inside new failure group
inNewFailure :: (Int -> CoreExpr) -> (CoreExpr -> CMonad a) -> CMonad a
inNewFailure fexp f = do
    fnum <- State $ cs -> let n = csNextFail cs in (n,cs {csNextFail = n+1})
    let exp = fexp fnum
    modify $ \ cs -> cs { csFails = exp : csFails cs }
    x <- f exp
    modify $ \ cs -> cs { csFails = tail $ csFails cs }
    return x

-- | retrieve the current failure
getFailExpr :: CMonad CoreExpr
getFailExpr = gets $ \ cs -> head $ csFails cs

-- | add a variable to the free list
addFree :: Id -> CMonad ()
addFree id = modify $ \ cs -> cs { csFree = Set.insert id (csFree cs) }

-- | test whether a variable is free
isFree :: Id -> CMonad ()
isFree id = gets $ \ cs -> Set.member id (csFree cs)

-- | bind a variable to a name
bind :: Id -> String -> CMonad ()
bind i s = modify $ \ cs -> cs { csBindings = (i,s) : csBindings cs }

-- | bind a 'real name', this fixes issues with lambdas
bindRealName :: Id -> CMonad String
bindRealName i = do
    state <- getState
    let modu = getModuleId state
        name = strIS state i
        name' = if "LAMBDA" `isPrefixOf` name then modu ++ "._" ++ name else name
    bind i name'

-- | bind a simple name
bindName :: Id -> CMonad String
bindName i = do { state <- getState ; bind i (strIS state i) }

-- | bind a constructor
bindCon :: Id -> CMonad String
bindCon i = do { state <- getState ; bind i (fixTuple $ strIS state i) }

-- | get the state
getState = gets csState

-- | fix a tuple to give it's name
fixTuple :: String -> String
fixTuple "()" = "Prelude.()"
fixTuple ('P':'r':'e':'l':'u':'d':'e':'.':xs)
    | all isDigit xs = "Prelude." ++ tupName (read xs)
    where
    tupName 1 = "1()"
    tupName n = "(" ++ replicate (n-1) ',' ++ ")"
fixTuple x = x



