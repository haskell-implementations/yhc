module YHC.Runtime.API(module YHC.Runtime.API, Node) where

import YHC.Primitive
import Foreign.C
import Foreign

--------------------------------------------------------------------------------------------------------------

-- an info, as seen by the API, implementation internal
data Info = Info InfoType (Ptr ())

-- a module
newtype Module = Module (Ptr ())

-- the type of an info
data InfoType = FInfo | PInfo | CInfo | XInfo
                deriving (Enum,Eq,Show)

-- representation of types
data TypeRep = TyCon String [TypeRep] | TyGeneric String
                deriving Show

data ConstItem = ConstSpace | ConstNode Node | ConstInfo Info
data ConstItemType = ConstTypeSpace | ConstTypeInfo | ConstTypeNode deriving (Enum,Eq)

type Byte = Char

--------------------------------------------------------------------------------------------------------------

lockInterpretter :: IO ()
lockInterpretter = primAPILock ()

unlockInterpretter :: IO ()
unlockInterpretter = primAPIUnlock ()

withLock :: IO a -> IO a
withLock f = do
  lockInterpretter
  x <- f
  unlockInterpretter
  return x

toNode :: a -> IO Node
toNode a = primAPIToNode (_E a)

withNode :: Node -> TypeRep -> (a -> IO b) -> IO b
#ifndef __HADDOCK__
withNode n typ f = do
  -- nTyp <- getNodeType n
  -- if unifyTypes nTyp typ then do
      (_E a) <- primAPIFromNode n
      f a
   --else
   --   error $ "withNode: incorrect type: "++show typ++" does not match "++show nTyp
#endif
getType :: a -> IO TypeRep
getType a = return $ error "getType not implememented"

getNodeType :: Node -> IO TypeRep
getNodeType n = return $ error "getNodeType not implemented"

getNodeInfo :: Node -> IO Info
getNodeInfo n = do
    ptr <- primAPINodeGetInfo n
    infoForPtr ptr

nodeSetArgs :: Node -> [Node] -> IO ()
nodeSetArgs node args = mapM_ (\(n,a) -> nodeSetArg node n a) (zip [0..] args)

nodeSetArg :: Node -> Int -> Node -> IO ()
nodeSetArg node n arg = primAPINodeSetArg node n arg

isCInfo :: Info -> Bool
isCInfo (Info t _) = t == CInfo

isFInfo :: Info -> Bool
isFInfo (Info t _) = t == FInfo

isPInfo :: Info -> Bool
isPInfo (Info t _) = t == PInfo

isXInfo :: Info -> Bool
isXInfo (Info t _) = t == XInfo

infoType :: Info -> InfoType
infoType (Info t _) = t

pinfoGetSize :: Info -> IO Int
pinfoGetSize (Info t p) = assertInfo t PInfo "pinfoGetSize" $ primAPIPInfoGetSize p

pinfoGetNeed :: Info -> IO Int
pinfoGetNeed (Info t p) = assertInfo t PInfo "pinfoGetNeed" $ primAPIPInfoGetNeed p

pinfoGetFInfo :: Info -> IO Info
pinfoGetFInfo (Info t p) = assertInfo t PInfo "pinfoGetFInfo" $ do
  ptr <- primAPIPInfoGetFInfo p
  infoForPtr ptr

finfoGetPInfo :: Info -> Int -> IO Info
finfoGetPInfo (Info t p) n = assertInfo t FInfo "finfoGetPInfo" $ do
  ptr <- primAPIFInfoGetPInfo p n
  infoForPtr ptr

finfoGetArity :: Info -> IO Int
finfoGetArity (Info t p) = assertInfo t FInfo "finfoGetArity" $ primAPIFInfoGetArity p

finfoGetStack :: Info -> IO Int
finfoGetStack (Info t p) = assertInfo t FInfo "finfoGetStack" $ primAPIFInfoGetStack p

finfoGetModule :: Info -> IO Module
finfoGetModule (Info t p) = assertInfo t FInfo "finfoGetModule" $ do
  ptr <- primAPIFInfoGetModule p
  return (Module ptr)

finfoGetName :: Info -> IO String
finfoGetName (Info t p) = assertInfo t FInfo "finfoGetName" $ do
  cs <- primAPIFInfoGetName p
  peekCString cs

finfoGetCode :: Info -> IO [Byte]
finfoGetCode (Info t p) = assertInfo t FInfo "finfoGetCode" $ do
    size <- primAPIFInfoGetCodeSize p
    cs <- primAPIFInfoGetCode p
    getBytes cs size
  where
  getBytes p 0 = return []
  getBytes p n = do
    b <- peek p
    bs <- getBytes (plusPtr p 1) (n-1)
    return (b:bs)

finfoGetConstTable :: Info -> IO [ConstItem]
finfoGetConstTable (Info t p) = assertInfo t FInfo "finfoGetConstTable" $ do
    num <- primAPIFInfoGetNumConsts p
    mapM (constItemAt p) [0..num-1]

cinfoGetSize :: Info -> IO Int
cinfoGetSize (Info t p) = assertInfo t CInfo "cinfoGetSize" $ primAPICInfoGetSize p

cinfoGetName :: Info -> IO String
cinfoGetName (Info t p) = assertInfo t CInfo "cinfoGetName" $ do
  cs <- primAPICInfoGetName p
  peekCString cs

cinfoGetModule :: Info -> IO Module
cinfoGetModule (Info t p) = assertInfo t CInfo "cinfoGetModule" $ do
  ptr <- primAPICInfoGetModule p
  return (Module ptr)

cinfoGetTag :: Info -> IO Int
cinfoGetTag (Info t p) = assertInfo t CInfo "cinfoGetTag" $ primAPICInfoGetTag p

asInt :: Node -> IO Int
asInt n = deNode n

asChar :: Node -> IO Char
asChar n = deNode n

asFloat :: Node -> IO Float
asFloat n = deNode n

asDouble :: Node -> IO Double
asDouble n = deNode n

asInteger :: Node -> IO Integer
asInteger n = deNode n

nodeGetArgs :: Node -> IO [Node]
nodeGetArgs node = withLock $ do
    info <- getNodeInfo node
    size <- if isCInfo info then cinfoGetSize info
                            else pinfoGetSize info
    mapM (nodeGetArg node) [0..size-1]

nodeGetArg :: Node -> Int -> IO Node
nodeGetArg node n = primAPINodeGetArg node n


----------------------------------------------------

newFInfo :: Int -> Int -> Module -> String -> [Byte] -> [ConstItem] -> IO Info
newFInfo arity stack (Module mod) name code consts =
  withCString name $ \ cname ->
  withCString code $ \ ccode -> do
    ptr <- primAPINewFInfo arity stack mod cname (length code) ccode (length consts)
    info <- infoForPtr ptr
    mapM_ (\(n,i) -> finfoSetConst info n i) (zip [0..] consts)
    return info

finfoSetConst :: Info -> Int -> ConstItem -> IO ()
finfoSetConst (Info t ptr) n item = assertInfo t FInfo "finfoSetConst" $
  case item of
    ConstInfo (Info _ i) -> primAPIFInfoSetConstInfo ptr n i
    ConstNode node       -> primAPIFInfoSetConstNode ptr n node
    ConstSpace           -> return ()

newCInfo :: Int -> Module -> String -> Int -> IO Info
newCInfo size (Module mod) name tag =
  withCString name $ \ cname -> do
  ptr <- primAPINewCInfo size mod cname tag
  infoForPtr ptr

newApNode :: Info -> [Node] -> IO Node
newApNode info@(Info t _) args = do
  inf <- case t of
            FInfo -> finfoGetPInfo info (length args)
            CInfo -> return $ info
  let (Info _ infoPtr) = inf
  node <- primAPINewNode infoPtr (length args)
  nodeSetArgs node args
  return node

----------------------------------------------------

isModuleLoaded :: String -> IO Bool
isModuleLoaded modname =
  withCString modname $ \ cmodname ->
    primAPIIsModuleLoaded cmodname

newModule :: String -> IO Module
newModule modname =
  withCString modname $ \ cmodname -> do
    ptr <- primAPINewModule cmodname
    return (Module ptr)

getModule :: String -> IO Module
getModule modname =
  withCString modname $ \ cmodname -> do
    ptr <- primAPIGetModule cmodname
    return (Module ptr)

moduleGetName :: Module -> IO String
moduleGetName (Module mptr) = do
  cs <- primAPIModuleGetName mptr
  peekCString cs

moduleLookupInfo :: Module -> String -> IO (Maybe Info)
moduleLookupInfo (Module mptr) item =
  withCString item $ \ citem -> do
  ptr <- primAPIModuleLookupInfo mptr citem
  if ptr == nullPtr then
    return Nothing
   else do
    info <- infoForPtr ptr
    return (Just info)

moduleLookupNode :: Module -> String -> IO (Maybe Node)
moduleLookupNode (Module mptr) item =
  withCString item $ \ citem -> do
  node <- primAPIModuleLookupNode mptr citem
  if primAPINodeIsNull node then
    return Nothing
   else
    return (Just node)

moduleLoad :: String -> IO (Maybe Module)
moduleLoad name =
  withCString name $ \ cname -> do
  mod <- primAPIModuleLoad cname
  if mod == nullPtr then
    return Nothing
   else
    return (Just $ Module mod)

----------------------------------------------------

deNode :: Node -> IO a
#ifndef __HADDOCK__
deNode n = do
  _E a <- primAPIFromNode n
  return a
#endif

infoForPtr :: Ptr () -> IO Info
infoForPtr ptr = do
  t <- primAPIInfoGetType ptr
  return $ Info (toEnum t) ptr

constItemAt :: Ptr () -> Int -> IO ConstItem
#ifndef __HADDOCK__
constItemAt i n = do
    t <- primAPIFInfoGetConstType i n
    let typ = toEnum t
    if typ == ConstTypeInfo then do
        ptr <- primAPIFInfoGetConstInfo i n
        info <- infoForPtr ptr
        return $ ConstInfo info
     else if typ == ConstTypeNode then do
        node <- primAPIFInfoGetConstNode i n
        return $ ConstNode node
      else error "constItemAt: neither Info nor Node!"
    where
    typ = toEnum $
#endif

assertInfo :: InfoType -> InfoType -> String -> a -> a
assertInfo x y fun a
    | x == y = a
    | otherwise = error $ "assertInfo: tried use '"++fun++"' which is a "++show y++" function on a "++show x

unifyTypes :: TypeRep -> TypeRep -> Bool
unifyTypes x y = True -- FIXME: !!

