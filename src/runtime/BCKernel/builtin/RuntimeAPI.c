#include "../hsffi.h"
#include "../module.h"
#include "../process.h"

/* take a value out of an _E box */
static Node* unbox(Node* box){
    Node* a = box->args[0];
    REMOVE_IND(a, Node*);
    return a;
}

/* put a value in to a _E box */
static Node* box(Node* value){
    Node* ret = (Node*)heap_alloc(wordsof(Node) + 1);
    MAKE_NODE(ret, G_infoEBox, N_NORMAL);
    ret->args[0] = value;
    return ret;
}

/* foreign import fastccall primAPILock :: () -> IO () */
void primAPILock(){ G_procSwitchDisabled = true; }

/* foreign import fastccall primAPIUnlock :: () -> IO () */
void primAPIUnlock(){ G_procSwitchDisabled = false; }

/* foreign import cast primAPIToNode :: _E a -> IO Node */
/* foreign import cast primAPIFromNode :: Node -> IO (_E a) */

/* foreign import fastccall primAPIGetInfo :: Node -> IO (Ptr ()) */
Info* primAPINodeGetInfo(Node* box){ return NODE_INFO(unbox(box)); }

/* foreign import fastccall primAPINodeGetArg :: Node -> Int -> IO Node */
Node* primAPINodeGetArg(Node* node, Int n){ return box(unbox(node)->args[n]); }

/* foreign import fastccall primAPINodeSetArg :: Node -> Int -> Node -> IO () */
void primAPINodeSetArg(Node* node, Int n, Node* arg){ unbox(node)->args[n] = unbox(arg); }

/* foreign import fastccall primAPINodeIsNull :: Node -> Bool */
Bool primAPINodeIsNull(Node* node){ return node == G_nodeUnit; }

/* foreign import fastccall primAPINewNode :: Ptr () -> Int -> IO Node */
Node* primAPINewNode(Info* info, Int size){
  Node* ret = (Node*)heap_alloc(wordsof(Node) + size);
  Int i;

  MAKE_NODE(ret, info, N_NORMAL);
  for (i = 0; i < size; i++){
    ret->args[i] = G_nodeUnit;
  }
  return box(ret);
}

/* foreign import fastccall primAPIInfoGetType :: Ptr () -> Int */
Int primAPIInfoGetType(Info* info){ return info->tag; }

/* foreign import fastccall primAPIPInfoGetSize :: Ptr () -> Int */
Int primAPIPInfoGetSize(PInfo* pinfo){ return pinfo->size; }

/* foreign import fastccall primAPIPInfoGetNeed :: Ptr () -> Int */
Int primAPIPInfoGetNeed(PInfo* pinfo){ return pinfo->need; }

/* foreign import fastccall primAPIPInfoGetFInfo :: Ptr () -> Ptr () */
FInfo* primAPIPInfoGetFInfo(PInfo* pinfo){ return PINFO_FINFO(pinfo); }

/* foreign import fastccall primAPIFInfoGetPInfo :: Ptr () -> Int -> Ptr () */
PInfo* primAPIFInfoGetPInfo(FInfo* finfo, Int n){ return &finfo->papTable[n]; }

/* foreign import fastccall primAPIFInfoGetArity :: Ptr () -> Int */
Int primAPIFInfoGetArity(FInfo* finfo){ return finfo->arity; }

/* foreign import fastccall primAPIFInfoGetStack :: Ptr () -> Int */
Int primAPIFInfoGetStack(FInfo* finfo){ return finfo->stack; }

/* foreign import fastccall primAPIFInfoGetModule :: Ptr () -> IO (Ptr ()) */
Module* primAPIFInfoGetModule(FInfo* finfo){ return finfo->module; }

/* foreign import fastccall primAPIFInfoGetName :: Ptr () -> CString */
Char* primAPIFInfoGetName(FInfo* finfo){ return finfo->name; }

/* foreign import fastccall primAPIFInfoGetCode :: Ptr () -> IO Int */
Int primAPIFInfoGetCodeSize(FInfo* finfo){ return finfo->codeSize; }

/* foreign import fastccall primAPIFInfoGetCode :: Ptr () -> IO (Ptr Char) */
CodePtr primAPIFInfoGetCode(FInfo* finfo){ return finfo->code; }

/* foreign import fastccall primAPIFInfoGetNumConsts :: Ptr () -> Int */
Int primAPIFInfoGetNumConsts(FInfo* finfo){ return finfo->numConsts; }

/* foreign import fastccall primAPIFInfoGetConstType :: Ptr () -> Int -> Int */
Int primAPIFInfoGetConstType(FInfo* finfo, Int n){ return finfo->constTypes[n]; }

/* foreign import fastccall primAPIFInfoGetConstInfo :: Ptr () -> Int -> Ptr () */
Info* primAPIFInfoGetConstInfo(FInfo* finfo, Int n){ return (Info*)finfo->constTable[n]; }

/* foreign import fastccall primAPIFInfoGetConstNode :: Ptr () -> Int -> Node */
Node* primAPIFInfoGetConstNode(FInfo* finfo, Int n){ return box((Node*)finfo->constTable[n]); }

/* foreign import fastccall primAPICInfoGetSize :: Ptr () -> Int */
Int primAPICInfoGetSize(CInfo* cinfo){ return cinfo->size; }

/* foreign import fastccall primAPICInfoGetModule :: Ptr () -> IO (Ptr ()) */
Module* primAPICInfoGetModule(CInfo* cinfo){ return cinfo->module; }

/* foreign import fastccall primAPICInfoGetName :: Ptr () -> CString */
Char* primAPICInfoGetName(CInfo* cinfo){ return cinfo->name; }

/* foreign import fastccall primAPICInfoGetTag :: Ptr () -> Int */
Int primAPICInfoGetTag(CInfo* cinfo){ return cinfo->number; }

/* foreign import fastccall primAPINewFInfo :: Int -> Int -> CString -> CString -> Int -> CString -> Int -> IO (Ptr ()) */
FInfo* primAPINewFInfo(Int arity, Int stack, Module* module, Char* name, Int codeSize, CodePtr code, Int numConsts){
  Object* obj = (Object*)malloc(sizeof(Object));
  UByte* constTypes = (UByte*)malloc(sizeof(UByte) * numConsts);
  ConstItem* constItems = (ConstItem*)malloc(sizeof(ConstItem) * numConsts);
  FInfo* finfo;
  CodePtr newCode = (CodePtr)malloc(sizeof(UByte) * codeSize);

  memcpy(newCode, code, codeSize);
  finfo = finfo_alloc(arity, stack, 0, strdup(name), codeSize, newCode, numConsts, constTypes, constItems, obj, module, NULL);
  hash_add(module->lookup, name, obj);
  return finfo;
}

/* foreign import fastccall primAPIFInfoSetConstInfo :: Ptr () -> Int -> Ptr () -> IO () */
void primAPIFInfoSetConstInfo(FInfo* finfo, int n, Info* i){
  finfo->constTypes[n] = C_INFO;
  finfo->constTable[n] = (ConstItem)i;
}

/* foreign import fastccall primAPIFInfoSetConstNode :: Ptr () -> Int -> Node -> IO () */
void primAPIFInfoSetConstNode(FInfo* finfo, int n, Node* node){
  finfo->constTypes[n] = C_NODE;
  finfo->constTable[n] = (ConstItem)node;
}

/* foreign import fastccall primAPINewCInfo :: Int -> CString -> CString -> Int -> IO (Ptr ()) */
CInfo* primAPINewCInfo(Int size, Module* mod, Char* name, Int tag){
  abort(); /* FIXME: !! */
  return NULL;
}

/* foreign import fastccall primAPIIsModuleLoaded :: CString -> IO Bool */
Bool primAPIIsModuleLoaded(Char* name){ return mod_isLoaded(name); }

/* foreign import fastccall primAPINewModule :: CString -> IO (Ptr ()) */
Module* primAPINewModule(Char* name){
  Module* ret = (Module*)malloc(sizeof(Module));

  ret->file = NULL;
  ret->name = strdup(name);
  ret->path = NULL;
  ret->numObjects = 0;
  ret->numStrings = 0;
  ret->strings = NULL;
  ret->objects = NULL;
  ret->lookup = hash_init();
  ret->external = NULL;
#ifdef HAT
  ret->hatModule = NULL;
#endif
  mod_linkModule(ret);
  return ret;
}

/* foreign import fastccall primAPIGetModule :: CString -> IO (Ptr ()) */
Module* primAPIGetModule(Char* name){ return mod_getModule(name); }

/* foreign import fastccall primAPIModuleGetName :: Ptr () -> IO CString */
Char* primAPIModuleGetName(Module* mod){ return mod->name; }

/* foreign import fastccall primAPIModuleLookupInfo :: Ptr () -> CString -> IO (Ptr ()) */
Info* primAPIModuleLookupInfo(Module* mod, Char* name){
  Object* obj = mod_resolve(mod->name, name);
  if (!obj){
    return NULL;
  }
  return obj->info;
}

/* foreign import fastccall primAPIModuleLookupNode :: Ptr () -> CString -> IO Node */
Node* primAPIModuleLookupNode(Module* mod, Char* name){
  Object* obj = mod_resolve(mod->name, name);
  if (!obj || !obj->node){
    return G_nodeUnit;
  }
  return box(obj->node);
}

/* foreign import fastccall primAPIModuleLoad :: CString -> IO (Ptr ()) */
Module* primAPIModuleLoad(Char* name){ return mod_load(name); }

void primRuntimeAPI_init(){
    prim_addFun("primAPILock", primAPILock);
    prim_addFun("primAPIUnlock", primAPIUnlock);

    prim_addFun("primAPINodeGetInfo", primAPINodeGetInfo);
    prim_addFun("primAPINodeGetArg", primAPINodeGetArg);
    prim_addFun("primAPINodeSetArg", primAPINodeSetArg);
    prim_addFun("primAPINodeIsNull", primAPINodeIsNull);
    prim_addFun("primAPINewNode", primAPINewNode);

    prim_addFun("primAPIInfoGetType", primAPIInfoGetType);
    prim_addFun("primAPIPInfoGetSize", primAPIPInfoGetSize);
    prim_addFun("primAPIPInfoGetNeed", primAPIPInfoGetNeed);
    prim_addFun("primAPIPInfoGetFInfo", primAPIPInfoGetFInfo);
    prim_addFun("primAPIFInfoGetPInfo", primAPIFInfoGetPInfo);
    prim_addFun("primAPIFInfoGetArity", primAPIFInfoGetArity);
    prim_addFun("primAPIFInfoGetStack", primAPIFInfoGetStack);
    prim_addFun("primAPIFInfoGetModule", primAPIFInfoGetModule);
    prim_addFun("primAPIFInfoGetName", primAPIFInfoGetName);
    prim_addFun("primAPIFInfoGetCodeSize", primAPIFInfoGetCodeSize);
    prim_addFun("primAPIFInfoGetCode", primAPIFInfoGetCode);
    prim_addFun("primAPIFInfoGetNumConsts", primAPIFInfoGetNumConsts);
    prim_addFun("primAPIFInfoGetConstType", primAPIFInfoGetConstType);
    prim_addFun("primAPIFInfoGetConstInfo", primAPIFInfoGetConstInfo);
    prim_addFun("primAPIFInfoGetConstNode", primAPIFInfoGetConstNode);
    prim_addFun("primAPICInfoGetSize", primAPICInfoGetSize);
    prim_addFun("primAPICInfoGetModule", primAPICInfoGetModule);
    prim_addFun("primAPICInfoGetName", primAPICInfoGetName);
    prim_addFun("primAPICInfoGetTag", primAPICInfoGetTag);
    prim_addFun("primAPINewFInfo", primAPINewFInfo);
    prim_addFun("primAPINewCInfo", primAPINewCInfo);
    prim_addFun("primAPIFInfoSetConstInfo", primAPIFInfoSetConstInfo);
    prim_addFun("primAPIFInfoSetConstNode", primAPIFInfoSetConstNode);

    prim_addFun("primAPIIsModuleLoaded", primAPIIsModuleLoaded);
    prim_addFun("primAPINewModule", primAPINewModule);
    prim_addFun("primAPIGetModule", primAPIGetModule);
    prim_addFun("primAPIModuleGetName", primAPIModuleGetName);
    prim_addFun("primAPIModuleLookupInfo", primAPIModuleLookupInfo);
    prim_addFun("primAPIModuleLookupNode", primAPIModuleLookupNode);
    prim_addFun("primAPIModuleLoad", primAPIModuleLoad);
}
