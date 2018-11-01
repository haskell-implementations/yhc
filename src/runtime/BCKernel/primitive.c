#include "platform.h"
#include "module.h"
#include "heap.h"
#include "integer.h"
#include "primitive.h"
#include <ffi.h>
#include "prelude.h"
#include "process.h"
#include "hsffi.h"

#define DEBUG 0

#if DEBUG
# define SHOW(x) x
#else
# define SHOW(x)
#endif

/* char* strdup(const char*); */

/*-----------------------------------------------------------------------------------------------*/

/* primitive CInfos */
CInfo G_infoInt;
CInfo G_infoLong;
CInfo G_infoFloat;
CInfo G_infoDouble;
CInfo G_infoChar;
CInfo G_infoInteger;
CInfo G_infoString;
CInfo G_infoPackedString;
CInfo G_infoBox;
CInfo G_infoForeignPtr;
CInfo G_infoCharArray;
CInfo G_infoArray;
CInfo G_infoProcStack;
CInfo G_infoMVar;
CInfo G_infoExceptionHandler;

/* finfos for byte code functions defined primitively */
FInfo* G_infoApply = NULL;
FInfo* G_infoPrimCString = NULL;
FInfo* G_infoUnpackPS = NULL;

/* infos for important prelude objects */
FInfo* G_infoBlackHole = NULL;
CInfo* G_infoCons = NULL;
CInfo* G_infoRight = NULL;
CInfo* G_infoJust = NULL;
CInfo* G_infoEBox = NULL;

/* tuple infos */
CInfo* G_infoTuple[MAX_TUPLE_SIZE];

/*-----------------------------------------------------------------------------------------------*/

/* allocate a byte code function from a code array */
FInfo* prim_allocBCFunc(Module* mod, Char* name, Int arity, Int stack, Int flags, Int codeSize, CodePtr code){
  FInfo* finfo;

  ObjectRef* objRef;

  objRef = (ObjectRef*)malloc(sizeof(ObjectRef));
  objRef->object = (Object*)malloc(sizeof(Object));
  finfo = finfo_alloc(arity, stack, flags, name, codeSize, code, 0, NULL, NULL, objRef->object, mod, NULL /* FIXME: position?*/);

  SHOW(printf("PRIM_BC %p:_Builtin.%s arity:%d\n", finfo, finfo->name, finfo->arity));
  hash_add(mod->lookup, strdup(name), objRef);
  return finfo;
}

static UInt8 apply_code[] = { NEED_HEAP_32, PUSH_ZAP_ARG_1, PUSH_ZAP_ARG_0, EVAL, APPLY_1, RETURN_EVAL, END_CODE };
static UInt8 cstring_code[] = { NEED_HEAP_32, PUSH_ZAP_ARG_0, STRING, RETURN, END_CODE };
static UInt8 catch_code[] = { NEED_HEAP_32, CATCH_BEGIN, 0x05, PUSH_ZAP_ARG_0, EVAL, CATCH_END,
                              RETURN, PUSH_ZAP_ARG_1, APPLY_1, RETURN_EVAL, END_CODE };
static UInt8 throw_code[] = { NEED_HEAP_32, PUSH_ZAP_ARG_0, THROW, END_CODE };
static UInt8 throwTo_code[] = { NEED_HEAP_32, PUSH_ZAP_ARG_1, PUSH_ZAP_ARG_0, THROW_TO, PUSH_ZAP_ARG_2, RETURN_EVAL, END_CODE };

/* load all the byte code functions */
void prim_loadBCFuncs(){
  /* add the _Builtin module */
  Module* mod = (Module*)malloc(sizeof(Module));
  mod->file = NULL;
  mod->name = "_Builtin";
  mod->path = NULL;
  mod->numObjects = 0;
  mod->numStrings = 0;
  mod->strings = NULL;
  mod->objects = NULL;
  mod->lookup = hash_init();
  mod->external = NULL;
#ifdef HAT
  mod->hatModule = NULL;
#endif
  mod_linkModule(mod);

  /* add the builtin functions */
  G_infoApply = prim_allocBCFunc(mod, "_apply", 2, 2, FFL_PRIM_APPLY, sizeof(apply_code), apply_code);
  G_infoPrimCString = prim_allocBCFunc(mod, "_primCString", 1, 1, FFL_NONE, sizeof(cstring_code), cstring_code);

  /* these two don't need to be accessed from the C side ... */
  prim_allocBCFunc(mod, "primCatch", 2, 16, FFL_NONE, sizeof(catch_code), catch_code);
  prim_allocBCFunc(mod, "primThrow", 1, 16, FFL_NONE, sizeof(throw_code), throw_code);
  prim_allocBCFunc(mod, "primThrowTo", 3, 16, FFL_NONE, sizeof(throwTo_code), throwTo_code);
}

/*-----------------------------------------------------------------------------------------------*/

/* important node defined inside the prelude */
Node* G_nodeNil = NULL;
Node* G_nodeUnit = NULL;
Node* G_nodeTrue = NULL;
Node* G_nodeFalse = NULL;
Node* G_nodeZapArg = NULL;
Node* G_nodeZapStack = NULL;
Node* G_nodeBlackHole = NULL;
Node* G_nodeNothing = NULL;

Global G_globalNil = { NULL, &G_nodeNil };
Global G_globalUnit = { NULL, &G_nodeUnit };
Global G_globalTrue = { NULL, &G_nodeTrue };
Global G_globalFalse = { NULL, &G_nodeFalse };
Global G_globalZapArg = { NULL, &G_nodeZapArg };
Global G_globalZapStack = { NULL, &G_nodeZapStack };
Global G_globalBlackHole = { NULL, &G_nodeBlackHole };
Global G_globalNothing = { NULL, &G_nodeNothing };

/* register all the global variable handles */
void prim_globals(){
  heap_pushGlobal(&G_globalNil);
  heap_pushGlobal(&G_globalUnit);
  heap_pushGlobal(&G_globalTrue);
  heap_pushGlobal(&G_globalFalse);
  heap_pushGlobal(&G_globalZapArg);
  heap_pushGlobal(&G_globalZapStack);
  heap_pushGlobal(&G_globalBlackHole);
  heap_pushGlobal(&G_globalNothing);
}


/*-----------------------------------------------------------------------------------------------*/

/* initialize a CInfo */
static void prim_initCInfo(CInfo* cinfo, Int words, char* name, Word flags, Module* prelude){
  cinfo->info.tag = I_CINFO;
  cinfo->size = words - wordsof(Node);
  cinfo->name = name;
  cinfo->number = 0;
  cinfo->flags = flags;

# ifdef HAT
  cinfo->hatInfo.tracePtr = 0;
  cinfo->hatInfo.module = prelude->hatModule;
  cinfo->hatInfo.name = name;
  cinfo->hatInfo.flags = HIFL_NONE;
# endif
}

/* initialize all the primitive infos */
static void prim_initInfos(Module* prelude){
/*
CInfo G_infoInt        = { { I_CINFO }, wordsof(INode) - wordsof(Node), "Int", 0, CI_NO_PTRS };
CInfo G_infoLong       = { { I_CINFO }, wordsof(LongNode) - wordsof(Node), "Long", 0, CI_NO_PTRS };
CInfo G_infoFloat      = { { I_CINFO }, wordsof(FloatNode) - wordsof(Node), "Float", 0, CI_NO_PTRS };
CInfo G_infoDouble     = { { I_CINFO }, wordsof(DoubleNode) - wordsof(Node), "Double", 0, CI_NO_PTRS };
CInfo G_infoChar       = { { I_CINFO }, wordsof(INode) - wordsof(Node), "Char", 0, CI_NO_PTRS };
CInfo G_infoInteger    = { { I_CINFO }, wordsof(IntegerNode) - wordsof(Node), "Integer", 0,
                           CI_NO_PTRS|CI_INTEGER };
CInfo G_infoString     = { { I_CINFO }, wordsof(StringNode) - wordsof(Node), "PrimString", 0, CI_NO_PTRS };
CInfo G_infoBox        = { { I_CINFO }, wordsof(BoxNode) - wordsof(Node), "Box", 0, CI_NO_PTRS };
CInfo G_infoForeignPtr = { { I_CINFO }, wordsof(ForeignPtrNode) - wordsof(Node), "ForeignPtr", 0,
                           CI_FOREIGN_PTR|CI_NO_PTRS };
CInfo G_infoCharArray  = { { I_CINFO }, 0, "CharArray", 0, CI_ARRAY | CI_NO_PTRS };
CInfo G_infoArray      = { { I_CINFO }, 0, "Array", 0, CI_ARRAY };
CInfo G_infoProcStack  = { { I_CINFO }, 0, "ProcStack", 0, CI_NO_PTRS | CI_PROC_STACK};
CInfo G_infoMVar       = { { I_CINFO }, wordsof(MVarNode) - wordsof(Node), "MVar", 0, CI_MVAR };
*/
  prim_initCInfo(&G_infoInt, wordsof(INode), "Int", CI_NO_PTRS, prelude);
  prim_initCInfo(&G_infoLong, wordsof(LongNode), "Long", CI_NO_PTRS, prelude);
  prim_initCInfo(&G_infoFloat, wordsof(FloatNode), "Float", CI_NO_PTRS, prelude);
  prim_initCInfo(&G_infoDouble, wordsof(DoubleNode), "Double", CI_NO_PTRS, prelude);
  prim_initCInfo(&G_infoChar, wordsof(INode), "Char", CI_NO_PTRS, prelude);
  prim_initCInfo(&G_infoInteger, wordsof(IntegerNode), "Integer", CI_NO_PTRS|CI_INTEGER, prelude);
  prim_initCInfo(&G_infoString, wordsof(StringNode), "PrimString", CI_NO_PTRS, prelude);
  prim_initCInfo(&G_infoPackedString, wordsof(Node), "PackedString", CI_ARRAY | CI_NO_PTRS, prelude);
  prim_initCInfo(&G_infoBox, wordsof(BoxNode), "FFIBox", CI_NO_PTRS, prelude);
  prim_initCInfo(&G_infoForeignPtr, wordsof(ForeignPtrNode), "ForeignPtr", CI_FOREIGN_PTR|CI_NO_PTRS, prelude);
  prim_initCInfo(&G_infoCharArray, wordsof(Node), "CharArray", CI_ARRAY | CI_NO_PTRS, prelude);
  prim_initCInfo(&G_infoArray, wordsof(Node), "Array", CI_ARRAY, prelude);
  prim_initCInfo(&G_infoProcStack, wordsof(Node), "ProcStack", CI_PROC_STACK, prelude);
  prim_initCInfo(&G_infoMVar, wordsof(MVarNode), "MVar", CI_MVAR, prelude);
  prim_initCInfo(&G_infoExceptionHandler, wordsof(ExceptionHandlerNode), "ExceptionHandler", CI_EXCEPTION_HANDLER, prelude);
}

/*-----------------------------------------------------------------------------------------------*/

Hashtable* G_primFuncs = NULL;
Module* G_preludeModule = NULL;

/* allocate a primitive constructor, these are added to the prelude directly */
void prim_addCon(Char* name, Int size, Int number, Int flags){
  ObjectRef* objRef;
  CInfo* cinfo;

  objRef = (ObjectRef*)malloc(sizeof(ObjectRef));
  objRef->object = (Object*)malloc(sizeof(Object));
  cinfo = cinfo_alloc(size, name, number, flags, objRef->object, G_preludeModule, NULL /* FIXME: position? */);

  SHOW(printf("PRIM_CON %p: Prelude.%s size:%d number:%d\n", cinfo, cinfo->name, cinfo->size, cinfo->number));

  hash_add(G_preludeModule->lookup, strdup(name), objRef);
}

/* allocate a primitive function */
void prim_addFun(Char* name, void* func){
  hash_add(G_primFuncs, name, func);
}

/* find a primitive function */
void* prim_lookupFun(Char* name){
  return hash_lookup(G_primFuncs, name);
}


/* module specific inits */
#ifdef NO_LIBFFI
void init_YHC_Primitive(WrapRegisterFun reg, void* arg);
#else
void primPrelude_init();
void primSystem_init();
void primIO_init();
void primIORef_init();
void primFFI_init();
void primArray_init();
void primPackedString_init();
void primConcurrent_init();
void primRuntimeAPI_init();
void primException_init();
#endif

/* register a primitive function */
void prim_register(const char* module, const char* name, void* fun, void* arg){
    // FIXME: dup the name and clean it up ...
    prim_addFun((Char*)name, fun);
}

/* load the primitive module */
void prim_load(Module* prelude){
  memset(G_infoTuple, 0, sizeof(G_infoTuple));
  G_primFuncs = hash_init();

  G_preludeModule = prelude;

  /* register global handles */
  prim_globals();

  /* initialize builtin infos */
  prim_initInfos(prelude);

  /* load the primitive functions */
#ifdef NO_LIBFFI
  init_YHC_Primitive(prim_register, NULL);
#else
  primPrelude_init();
  primIO_init();
  primIORef_init();
  primFFI_init();
  primSystem_init();
  primPackedString_init();
  primConcurrent_init();
  primArray_init();
  primRuntimeAPI_init();
  primException_init();
#endif

  /* there's no way to write a one tuple in haskell but we need it for dictionaries */
  prim_addCon("1()", 1, 0, CI_NONE);

  /* load the primitive bytecode functions */
  prim_loadBCFuncs();
}

