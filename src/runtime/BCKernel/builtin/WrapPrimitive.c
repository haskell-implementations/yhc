#include "../hsffi.h"

void* primAPIModuleLoad(void*);

/* auto-generated wrapper for YHC.Primitive.primAPIModuleLoad##X */
Node* Wrap_primAPIModuleLoad(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAPIModuleLoad(UNBOX_PTR(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

Node* primAPIModuleLookupNode(void*,void*);

/* auto-generated wrapper for YHC.Primitive.primAPIModuleLookupNode##X */
Node* Wrap_primAPIModuleLookupNode(Node* node){
  Node* nResult = NULL;
  Node* pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primAPIModuleLookupNode(UNBOX_PTR(arg0),UNBOX_PTR(arg1));
  BOX_UNKNOWN(nResult,pResult);
  return nResult;
}

void* primAPIModuleLookupInfo(void*,void*);

/* auto-generated wrapper for YHC.Primitive.primAPIModuleLookupInfo##X */
Node* Wrap_primAPIModuleLookupInfo(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primAPIModuleLookupInfo(UNBOX_PTR(arg0),UNBOX_PTR(arg1));
  BOX_PTR(nResult,pResult);
  return nResult;
}

void* primAPIModuleGetName(void*);

/* auto-generated wrapper for YHC.Primitive.primAPIModuleGetName##X */
Node* Wrap_primAPIModuleGetName(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAPIModuleGetName(UNBOX_PTR(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

void* primAPIGetModule(void*);

/* auto-generated wrapper for YHC.Primitive.primAPIGetModule##X */
Node* Wrap_primAPIGetModule(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAPIGetModule(UNBOX_PTR(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

void* primAPINewModule(void*);

/* auto-generated wrapper for YHC.Primitive.primAPINewModule##X */
Node* Wrap_primAPINewModule(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAPINewModule(UNBOX_PTR(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

HsBool primAPIIsModuleLoaded(void*);

/* auto-generated wrapper for YHC.Primitive.primAPIIsModuleLoaded##X */
Node* Wrap_primAPIIsModuleLoaded(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAPIIsModuleLoaded(UNBOX_PTR(arg0));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

void* primAPINewCInfo(HsInt,void*,void*,HsInt);

/* auto-generated wrapper for YHC.Primitive.primAPINewCInfo##X */
Node* Wrap_primAPINewCInfo(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  Node* arg2 = node->args[2];
  Node* arg3 = node->args[3];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  REMOVE_IND(arg2, Node*);
  REMOVE_IND(arg3, Node*);
  pResult = primAPINewCInfo(UNBOX_INT(arg0),UNBOX_PTR(arg1),UNBOX_PTR(arg2),UNBOX_INT(arg3));
  BOX_PTR(nResult,pResult);
  return nResult;
}

void primAPIFInfoSetConstNode(void*,HsInt,Node*);

/* auto-generated wrapper for YHC.Primitive.primAPIFInfoSetConstNode##X */
Node* Wrap_primAPIFInfoSetConstNode(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  Node* arg2 = node->args[2];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  REMOVE_IND(arg2, Node*);
  primAPIFInfoSetConstNode(UNBOX_PTR(arg0),UNBOX_INT(arg1),UNBOX_UNKNOWN(arg2));
  nResult = NODE_UNIT;
  return nResult;
}

void primAPIFInfoSetConstInfo(void*,HsInt,void*);

/* auto-generated wrapper for YHC.Primitive.primAPIFInfoSetConstInfo##X */
Node* Wrap_primAPIFInfoSetConstInfo(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  Node* arg2 = node->args[2];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  REMOVE_IND(arg2, Node*);
  primAPIFInfoSetConstInfo(UNBOX_PTR(arg0),UNBOX_INT(arg1),UNBOX_PTR(arg2));
  nResult = NODE_UNIT;
  return nResult;
}

void* primAPINewFInfo(HsInt,HsInt,void*,void*,HsInt,void*,HsInt);

/* auto-generated wrapper for YHC.Primitive.primAPINewFInfo##X */
Node* Wrap_primAPINewFInfo(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  Node* arg2 = node->args[2];
  Node* arg3 = node->args[3];
  Node* arg4 = node->args[4];
  Node* arg5 = node->args[5];
  Node* arg6 = node->args[6];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  REMOVE_IND(arg2, Node*);
  REMOVE_IND(arg3, Node*);
  REMOVE_IND(arg4, Node*);
  REMOVE_IND(arg5, Node*);
  REMOVE_IND(arg6, Node*);
  pResult = primAPINewFInfo(UNBOX_INT(arg0),UNBOX_INT(arg1),UNBOX_PTR(arg2),UNBOX_PTR(arg3),UNBOX_INT(arg4),UNBOX_PTR(arg5),UNBOX_INT(arg6));
  BOX_PTR(nResult,pResult);
  return nResult;
}

HsInt primAPICInfoGetTag(void*);

/* auto-generated wrapper for YHC.Primitive.primAPICInfoGetTag##X */
Node* Wrap_primAPICInfoGetTag(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAPICInfoGetTag(UNBOX_PTR(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

void* primAPICInfoGetName(void*);

/* auto-generated wrapper for YHC.Primitive.primAPICInfoGetName##X */
Node* Wrap_primAPICInfoGetName(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAPICInfoGetName(UNBOX_PTR(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

void* primAPICInfoGetModule(void*);

/* auto-generated wrapper for YHC.Primitive.primAPICInfoGetModule##X */
Node* Wrap_primAPICInfoGetModule(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAPICInfoGetModule(UNBOX_PTR(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

HsInt primAPICInfoGetSize(void*);

/* auto-generated wrapper for YHC.Primitive.primAPICInfoGetSize##X */
Node* Wrap_primAPICInfoGetSize(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAPICInfoGetSize(UNBOX_PTR(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

Node* primAPIFInfoGetConstNode(void*,HsInt);

/* auto-generated wrapper for YHC.Primitive.primAPIFInfoGetConstNode##X */
Node* Wrap_primAPIFInfoGetConstNode(Node* node){
  Node* nResult = NULL;
  Node* pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primAPIFInfoGetConstNode(UNBOX_PTR(arg0),UNBOX_INT(arg1));
  BOX_UNKNOWN(nResult,pResult);
  return nResult;
}

void* primAPIFInfoGetConstInfo(void*,HsInt);

/* auto-generated wrapper for YHC.Primitive.primAPIFInfoGetConstInfo##X */
Node* Wrap_primAPIFInfoGetConstInfo(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primAPIFInfoGetConstInfo(UNBOX_PTR(arg0),UNBOX_INT(arg1));
  BOX_PTR(nResult,pResult);
  return nResult;
}

HsInt primAPIFInfoGetConstType(void*,HsInt);

/* auto-generated wrapper for YHC.Primitive.primAPIFInfoGetConstType##X */
Node* Wrap_primAPIFInfoGetConstType(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primAPIFInfoGetConstType(UNBOX_PTR(arg0),UNBOX_INT(arg1));
  BOX_INT(nResult,pResult);
  return nResult;
}

HsInt primAPIFInfoGetNumConsts(void*);

/* auto-generated wrapper for YHC.Primitive.primAPIFInfoGetNumConsts##X */
Node* Wrap_primAPIFInfoGetNumConsts(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAPIFInfoGetNumConsts(UNBOX_PTR(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

void* primAPIFInfoGetCode(void*);

/* auto-generated wrapper for YHC.Primitive.primAPIFInfoGetCode##X */
Node* Wrap_primAPIFInfoGetCode(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAPIFInfoGetCode(UNBOX_PTR(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

HsInt primAPIFInfoGetCodeSize(void*);

/* auto-generated wrapper for YHC.Primitive.primAPIFInfoGetCodeSize##X */
Node* Wrap_primAPIFInfoGetCodeSize(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAPIFInfoGetCodeSize(UNBOX_PTR(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

void* primAPIFInfoGetName(void*);

/* auto-generated wrapper for YHC.Primitive.primAPIFInfoGetName##X */
Node* Wrap_primAPIFInfoGetName(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAPIFInfoGetName(UNBOX_PTR(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

void* primAPIFInfoGetModule(void*);

/* auto-generated wrapper for YHC.Primitive.primAPIFInfoGetModule##X */
Node* Wrap_primAPIFInfoGetModule(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAPIFInfoGetModule(UNBOX_PTR(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

HsInt primAPIFInfoGetStack(void*);

/* auto-generated wrapper for YHC.Primitive.primAPIFInfoGetStack##X */
Node* Wrap_primAPIFInfoGetStack(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAPIFInfoGetStack(UNBOX_PTR(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

HsInt primAPIFInfoGetArity(void*);

/* auto-generated wrapper for YHC.Primitive.primAPIFInfoGetArity##X */
Node* Wrap_primAPIFInfoGetArity(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAPIFInfoGetArity(UNBOX_PTR(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

void* primAPIFInfoGetPInfo(void*,HsInt);

/* auto-generated wrapper for YHC.Primitive.primAPIFInfoGetPInfo##X */
Node* Wrap_primAPIFInfoGetPInfo(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primAPIFInfoGetPInfo(UNBOX_PTR(arg0),UNBOX_INT(arg1));
  BOX_PTR(nResult,pResult);
  return nResult;
}

void* primAPIPInfoGetFInfo(void*);

/* auto-generated wrapper for YHC.Primitive.primAPIPInfoGetFInfo##X */
Node* Wrap_primAPIPInfoGetFInfo(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAPIPInfoGetFInfo(UNBOX_PTR(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

HsInt primAPIPInfoGetNeed(void*);

/* auto-generated wrapper for YHC.Primitive.primAPIPInfoGetNeed##X */
Node* Wrap_primAPIPInfoGetNeed(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAPIPInfoGetNeed(UNBOX_PTR(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

HsInt primAPIPInfoGetSize(void*);

/* auto-generated wrapper for YHC.Primitive.primAPIPInfoGetSize##X */
Node* Wrap_primAPIPInfoGetSize(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAPIPInfoGetSize(UNBOX_PTR(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

HsInt primAPIInfoGetType(void*);

/* auto-generated wrapper for YHC.Primitive.primAPIInfoGetType##X */
Node* Wrap_primAPIInfoGetType(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAPIInfoGetType(UNBOX_PTR(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

Node* primAPINewNode(void*,HsInt);

/* auto-generated wrapper for YHC.Primitive.primAPINewNode##X */
Node* Wrap_primAPINewNode(Node* node){
  Node* nResult = NULL;
  Node* pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primAPINewNode(UNBOX_PTR(arg0),UNBOX_INT(arg1));
  BOX_UNKNOWN(nResult,pResult);
  return nResult;
}

HsBool primAPINodeIsNull(Node*);

/* auto-generated wrapper for YHC.Primitive.primAPINodeIsNull#X */
Node* Wrap_primAPINodeIsNull(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAPINodeIsNull(UNBOX_UNKNOWN(arg0));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

void primAPINodeSetArg(Node*,HsInt,Node*);

/* auto-generated wrapper for YHC.Primitive.primAPINodeSetArg##X */
Node* Wrap_primAPINodeSetArg(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  Node* arg2 = node->args[2];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  REMOVE_IND(arg2, Node*);
  primAPINodeSetArg(UNBOX_UNKNOWN(arg0),UNBOX_INT(arg1),UNBOX_UNKNOWN(arg2));
  nResult = NODE_UNIT;
  return nResult;
}

Node* primAPINodeGetArg(Node*,HsInt);

/* auto-generated wrapper for YHC.Primitive.primAPINodeGetArg##X */
Node* Wrap_primAPINodeGetArg(Node* node){
  Node* nResult = NULL;
  Node* pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primAPINodeGetArg(UNBOX_UNKNOWN(arg0),UNBOX_INT(arg1));
  BOX_UNKNOWN(nResult,pResult);
  return nResult;
}

void* primAPINodeGetInfo(Node*);

/* auto-generated wrapper for YHC.Primitive.primAPINodeGetInfo##X */
Node* Wrap_primAPINodeGetInfo(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAPINodeGetInfo(UNBOX_UNKNOWN(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

/* auto-generated wrapper for YHC.Primitive.primAPIFromNode##X */
Node* Wrap_primAPIFromNode(Node* node){
  Node* nResult = NULL;
  Node* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = (Node*)(UNBOX_UNKNOWN(arg0));
  BOX_UNKNOWN(nResult,pResult);
  return nResult;
}

/* auto-generated wrapper for YHC.Primitive.primAPIToNode##X */
Node* Wrap_primAPIToNode(Node* node){
  Node* nResult = NULL;
  Node* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = (Node*)(UNBOX_UNKNOWN(arg0));
  BOX_UNKNOWN(nResult,pResult);
  return nResult;
}

void primAPIUnlock(void);

/* auto-generated wrapper for YHC.Primitive.primAPIUnlock##X */
Node* Wrap_primAPIUnlock(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  primAPIUnlock(UNBOX_UNIT(arg0));
  nResult = NODE_UNIT;
  return nResult;
}

void primAPILock(void);

/* auto-generated wrapper for YHC.Primitive.primAPILock##X */
Node* Wrap_primAPILock(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  primAPILock(UNBOX_UNIT(arg0));
  nResult = NODE_UNIT;
  return nResult;
}

void primWriteInt64AtAddr(void*,HsInt64);

/* auto-generated wrapper for YHC.Primitive.primWriteInt64AtAddr##X */
Node* Wrap_primWriteInt64AtAddr(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  primWriteInt64AtAddr(UNBOX_PTR(arg0),UNBOX_INT64(arg1));
  nResult = NODE_UNIT;
  return nResult;
}

HsInt64 primReadInt64AtAddr(void*);

/* auto-generated wrapper for YHC.Primitive.primReadInt64AtAddr##X */
Node* Wrap_primReadInt64AtAddr(Node* node){
  Node* nResult = NULL;
  HsInt64 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primReadInt64AtAddr(UNBOX_PTR(arg0));
  BOX_INT64(nResult,pResult);
  return nResult;
}

void primWriteInt32AtAddr(void*,HsInt32);

/* auto-generated wrapper for YHC.Primitive.primWriteInt32AtAddr##X */
Node* Wrap_primWriteInt32AtAddr(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  primWriteInt32AtAddr(UNBOX_PTR(arg0),UNBOX_INT32(arg1));
  nResult = NODE_UNIT;
  return nResult;
}

HsInt32 primReadInt32AtAddr(void*);

/* auto-generated wrapper for YHC.Primitive.primReadInt32AtAddr##X */
Node* Wrap_primReadInt32AtAddr(Node* node){
  Node* nResult = NULL;
  HsInt32 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primReadInt32AtAddr(UNBOX_PTR(arg0));
  BOX_INT32(nResult,pResult);
  return nResult;
}

void primWriteInt16AtAddr(void*,HsInt8);

/* auto-generated wrapper for YHC.Primitive.primWriteInt16AtAddr##X */
Node* Wrap_primWriteInt16AtAddr(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  primWriteInt16AtAddr(UNBOX_PTR(arg0),UNBOX_INT8(arg1));
  nResult = NODE_UNIT;
  return nResult;
}

HsInt8 primReadInt16AtAddr(void*);

/* auto-generated wrapper for YHC.Primitive.primReadInt16AtAddr##X */
Node* Wrap_primReadInt16AtAddr(Node* node){
  Node* nResult = NULL;
  HsInt8 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primReadInt16AtAddr(UNBOX_PTR(arg0));
  BOX_INT8(nResult,pResult);
  return nResult;
}

void primWriteInt8AtAddr(void*,HsInt8);

/* auto-generated wrapper for YHC.Primitive.primWriteInt8AtAddr##X */
Node* Wrap_primWriteInt8AtAddr(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  primWriteInt8AtAddr(UNBOX_PTR(arg0),UNBOX_INT8(arg1));
  nResult = NODE_UNIT;
  return nResult;
}

HsInt8 primReadInt8AtAddr(void*);

/* auto-generated wrapper for YHC.Primitive.primReadInt8AtAddr##X */
Node* Wrap_primReadInt8AtAddr(Node* node){
  Node* nResult = NULL;
  HsInt8 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primReadInt8AtAddr(UNBOX_PTR(arg0));
  BOX_INT8(nResult,pResult);
  return nResult;
}

void primWriteWord64AtAddr(void*,HsWord64);

/* auto-generated wrapper for YHC.Primitive.primWriteWord64AtAddr##X */
Node* Wrap_primWriteWord64AtAddr(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  primWriteWord64AtAddr(UNBOX_PTR(arg0),UNBOX_WORD64(arg1));
  nResult = NODE_UNIT;
  return nResult;
}

HsWord64 primReadWord64AtAddr(void*);

/* auto-generated wrapper for YHC.Primitive.primReadWord64AtAddr##X */
Node* Wrap_primReadWord64AtAddr(Node* node){
  Node* nResult = NULL;
  HsWord64 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primReadWord64AtAddr(UNBOX_PTR(arg0));
  BOX_WORD64(nResult,pResult);
  return nResult;
}

void primWriteWord32AtAddr(void*,HsWord32);

/* auto-generated wrapper for YHC.Primitive.primWriteWord32AtAddr##X */
Node* Wrap_primWriteWord32AtAddr(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  primWriteWord32AtAddr(UNBOX_PTR(arg0),UNBOX_WORD32(arg1));
  nResult = NODE_UNIT;
  return nResult;
}

HsWord32 primReadWord32AtAddr(void*);

/* auto-generated wrapper for YHC.Primitive.primReadWord32AtAddr##X */
Node* Wrap_primReadWord32AtAddr(Node* node){
  Node* nResult = NULL;
  HsWord32 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primReadWord32AtAddr(UNBOX_PTR(arg0));
  BOX_WORD32(nResult,pResult);
  return nResult;
}

void primWriteWord16AtAddr(void*,HsWord16);

/* auto-generated wrapper for YHC.Primitive.primWriteWord16AtAddr##X */
Node* Wrap_primWriteWord16AtAddr(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  primWriteWord16AtAddr(UNBOX_PTR(arg0),UNBOX_WORD16(arg1));
  nResult = NODE_UNIT;
  return nResult;
}

HsWord16 primReadWord16AtAddr(void*);

/* auto-generated wrapper for YHC.Primitive.primReadWord16AtAddr##X */
Node* Wrap_primReadWord16AtAddr(Node* node){
  Node* nResult = NULL;
  HsWord16 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primReadWord16AtAddr(UNBOX_PTR(arg0));
  BOX_WORD16(nResult,pResult);
  return nResult;
}

void primWriteWord8AtAddr(void*,HsWord8);

/* auto-generated wrapper for YHC.Primitive.primWriteWord8AtAddr##X */
Node* Wrap_primWriteWord8AtAddr(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  primWriteWord8AtAddr(UNBOX_PTR(arg0),UNBOX_WORD8(arg1));
  nResult = NODE_UNIT;
  return nResult;
}

HsWord8 primReadWord8AtAddr(void*);

/* auto-generated wrapper for YHC.Primitive.primReadWord8AtAddr##X */
Node* Wrap_primReadWord8AtAddr(Node* node){
  Node* nResult = NULL;
  HsWord8 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primReadWord8AtAddr(UNBOX_PTR(arg0));
  BOX_WORD8(nResult,pResult);
  return nResult;
}

void primWriteAddrAtAddr(void*,void*);

/* auto-generated wrapper for YHC.Primitive.primWriteAddrAtAddr##X */
Node* Wrap_primWriteAddrAtAddr(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  primWriteAddrAtAddr(UNBOX_PTR(arg0),UNBOX_PTR(arg1));
  nResult = NODE_UNIT;
  return nResult;
}

void* primReadAddrAtAddr(void*);

/* auto-generated wrapper for YHC.Primitive.primReadAddrAtAddr##X */
Node* Wrap_primReadAddrAtAddr(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primReadAddrAtAddr(UNBOX_PTR(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

void primWriteDoubleAtAddr(void*,double);

/* auto-generated wrapper for YHC.Primitive.primWriteDoubleAtAddr##X */
Node* Wrap_primWriteDoubleAtAddr(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  primWriteDoubleAtAddr(UNBOX_PTR(arg0),UNBOX_DOUBLE(arg1));
  nResult = NODE_UNIT;
  return nResult;
}

double primReadDoubleAtAddr(void*);

/* auto-generated wrapper for YHC.Primitive.primReadDoubleAtAddr##X */
Node* Wrap_primReadDoubleAtAddr(Node* node){
  Node* nResult = NULL;
  double pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primReadDoubleAtAddr(UNBOX_PTR(arg0));
  BOX_DOUBLE(nResult,pResult);
  return nResult;
}

void primWriteFloatAtAddr(void*,float);

/* auto-generated wrapper for YHC.Primitive.primWriteFloatAtAddr##X */
Node* Wrap_primWriteFloatAtAddr(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  primWriteFloatAtAddr(UNBOX_PTR(arg0),UNBOX_FLOAT(arg1));
  nResult = NODE_UNIT;
  return nResult;
}

float primReadFloatAtAddr(void*);

/* auto-generated wrapper for YHC.Primitive.primReadFloatAtAddr##X */
Node* Wrap_primReadFloatAtAddr(Node* node){
  Node* nResult = NULL;
  float pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primReadFloatAtAddr(UNBOX_PTR(arg0));
  BOX_FLOAT(nResult,pResult);
  return nResult;
}

void primWriteIntAtAddr(void*,HsInt);

/* auto-generated wrapper for YHC.Primitive.primWriteIntAtAddr##X */
Node* Wrap_primWriteIntAtAddr(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  primWriteIntAtAddr(UNBOX_PTR(arg0),UNBOX_INT(arg1));
  nResult = NODE_UNIT;
  return nResult;
}

HsInt primReadIntAtAddr(void*);

/* auto-generated wrapper for YHC.Primitive.primReadIntAtAddr##X */
Node* Wrap_primReadIntAtAddr(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primReadIntAtAddr(UNBOX_PTR(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

void primWriteCharAtAddr(void*,char);

/* auto-generated wrapper for YHC.Primitive.primWriteCharAtAddr##X */
Node* Wrap_primWriteCharAtAddr(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  primWriteCharAtAddr(UNBOX_PTR(arg0),UNBOX_CHAR(arg1));
  nResult = NODE_UNIT;
  return nResult;
}

char primReadCharAtAddr(void*);

/* auto-generated wrapper for YHC.Primitive.primReadCharAtAddr##X */
Node* Wrap_primReadCharAtAddr(Node* node){
  Node* nResult = NULL;
  char pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primReadCharAtAddr(UNBOX_PTR(arg0));
  BOX_CHAR(nResult,pResult);
  return nResult;
}

Node* primWord64ToInteger(HsWord64);

/* auto-generated wrapper for YHC.Primitive.primWord64ToInteger#X */
Node* Wrap_primWord64ToInteger(Node* node){
  Node* nResult = NULL;
  Node* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primWord64ToInteger(UNBOX_WORD64(arg0));
  BOX_INTEGER(nResult,pResult);
  return nResult;
}

HsWord64 primWord64FromInteger(Node*);

/* auto-generated wrapper for YHC.Primitive.primWord64FromInteger#X */
Node* Wrap_primWord64FromInteger(Node* node){
  Node* nResult = NULL;
  HsWord64 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primWord64FromInteger(UNBOX_INTEGER(arg0));
  BOX_WORD64(nResult,pResult);
  return nResult;
}

HsInt primFromEnumWord64(HsWord64);

/* auto-generated wrapper for YHC.Primitive.primFromEnumWord64#X */
Node* Wrap_primFromEnumWord64(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primFromEnumWord64(UNBOX_WORD64(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

HsWord64 primToEnumWord64(HsInt);

/* auto-generated wrapper for YHC.Primitive.primToEnumWord64#X */
Node* Wrap_primToEnumWord64(Node* node){
  Node* nResult = NULL;
  HsWord64 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primToEnumWord64(UNBOX_INT(arg0));
  BOX_WORD64(nResult,pResult);
  return nResult;
}

HsWord64 primRemWord64(HsWord64,HsWord64);

/* auto-generated wrapper for YHC.Primitive.primRemWord64#X */
Node* Wrap_primRemWord64(Node* node){
  Node* nResult = NULL;
  HsWord64 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primRemWord64(UNBOX_WORD64(arg0),UNBOX_WORD64(arg1));
  BOX_WORD64(nResult,pResult);
  return nResult;
}

HsWord64 primQuotWord64(HsWord64,HsWord64);

/* auto-generated wrapper for YHC.Primitive.primQuotWord64#X */
Node* Wrap_primQuotWord64(Node* node){
  Node* nResult = NULL;
  HsWord64 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primQuotWord64(UNBOX_WORD64(arg0),UNBOX_WORD64(arg1));
  BOX_WORD64(nResult,pResult);
  return nResult;
}

HsWord64 primSignumWord64(HsWord64);

/* auto-generated wrapper for YHC.Primitive.primSignumWord64#X */
Node* Wrap_primSignumWord64(Node* node){
  Node* nResult = NULL;
  HsWord64 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primSignumWord64(UNBOX_WORD64(arg0));
  BOX_WORD64(nResult,pResult);
  return nResult;
}

HsWord64 primAbsWord64(HsWord64);

/* auto-generated wrapper for YHC.Primitive.primAbsWord64#X */
Node* Wrap_primAbsWord64(Node* node){
  Node* nResult = NULL;
  HsWord64 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAbsWord64(UNBOX_WORD64(arg0));
  BOX_WORD64(nResult,pResult);
  return nResult;
}

HsWord64 primMulWord64(HsWord64,HsWord64);

/* auto-generated wrapper for YHC.Primitive.primMulWord64#X */
Node* Wrap_primMulWord64(Node* node){
  Node* nResult = NULL;
  HsWord64 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primMulWord64(UNBOX_WORD64(arg0),UNBOX_WORD64(arg1));
  BOX_WORD64(nResult,pResult);
  return nResult;
}

HsWord64 primSubWord64(HsWord64,HsWord64);

/* auto-generated wrapper for YHC.Primitive.primSubWord64#X */
Node* Wrap_primSubWord64(Node* node){
  Node* nResult = NULL;
  HsWord64 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primSubWord64(UNBOX_WORD64(arg0),UNBOX_WORD64(arg1));
  BOX_WORD64(nResult,pResult);
  return nResult;
}

HsWord64 primAddWord64(HsWord64,HsWord64);

/* auto-generated wrapper for YHC.Primitive.primAddWord64#X */
Node* Wrap_primAddWord64(Node* node){
  Node* nResult = NULL;
  HsWord64 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primAddWord64(UNBOX_WORD64(arg0),UNBOX_WORD64(arg1));
  BOX_WORD64(nResult,pResult);
  return nResult;
}

HsBool primGeWord64(HsWord64,HsWord64);

/* auto-generated wrapper for YHC.Primitive.primGeWord64#X */
Node* Wrap_primGeWord64(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primGeWord64(UNBOX_WORD64(arg0),UNBOX_WORD64(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primGtWord64(HsWord64,HsWord64);

/* auto-generated wrapper for YHC.Primitive.primGtWord64#X */
Node* Wrap_primGtWord64(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primGtWord64(UNBOX_WORD64(arg0),UNBOX_WORD64(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primLeWord64(HsWord64,HsWord64);

/* auto-generated wrapper for YHC.Primitive.primLeWord64#X */
Node* Wrap_primLeWord64(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primLeWord64(UNBOX_WORD64(arg0),UNBOX_WORD64(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primLtWord64(HsWord64,HsWord64);

/* auto-generated wrapper for YHC.Primitive.primLtWord64#X */
Node* Wrap_primLtWord64(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primLtWord64(UNBOX_WORD64(arg0),UNBOX_WORD64(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primEqWord64(HsWord64,HsWord64);

/* auto-generated wrapper for YHC.Primitive.primEqWord64#X */
Node* Wrap_primEqWord64(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primEqWord64(UNBOX_WORD64(arg0),UNBOX_WORD64(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

Node* primWord32ToInteger(HsWord32);

/* auto-generated wrapper for YHC.Primitive.primWord32ToInteger#X */
Node* Wrap_primWord32ToInteger(Node* node){
  Node* nResult = NULL;
  Node* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primWord32ToInteger(UNBOX_WORD32(arg0));
  BOX_INTEGER(nResult,pResult);
  return nResult;
}

HsWord32 primWord32FromInteger(Node*);

/* auto-generated wrapper for YHC.Primitive.primWord32FromInteger#X */
Node* Wrap_primWord32FromInteger(Node* node){
  Node* nResult = NULL;
  HsWord32 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primWord32FromInteger(UNBOX_INTEGER(arg0));
  BOX_WORD32(nResult,pResult);
  return nResult;
}

HsInt primFromEnumWord32(HsWord32);

/* auto-generated wrapper for YHC.Primitive.primFromEnumWord32#X */
Node* Wrap_primFromEnumWord32(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primFromEnumWord32(UNBOX_WORD32(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

HsWord32 primToEnumWord32(HsInt);

/* auto-generated wrapper for YHC.Primitive.primToEnumWord32#X */
Node* Wrap_primToEnumWord32(Node* node){
  Node* nResult = NULL;
  HsWord32 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primToEnumWord32(UNBOX_INT(arg0));
  BOX_WORD32(nResult,pResult);
  return nResult;
}

HsWord32 primRemWord32(HsWord32,HsWord32);

/* auto-generated wrapper for YHC.Primitive.primRemWord32#X */
Node* Wrap_primRemWord32(Node* node){
  Node* nResult = NULL;
  HsWord32 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primRemWord32(UNBOX_WORD32(arg0),UNBOX_WORD32(arg1));
  BOX_WORD32(nResult,pResult);
  return nResult;
}

HsWord32 primQuotWord32(HsWord32,HsWord32);

/* auto-generated wrapper for YHC.Primitive.primQuotWord32#X */
Node* Wrap_primQuotWord32(Node* node){
  Node* nResult = NULL;
  HsWord32 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primQuotWord32(UNBOX_WORD32(arg0),UNBOX_WORD32(arg1));
  BOX_WORD32(nResult,pResult);
  return nResult;
}

HsWord32 primSignumWord32(HsWord32);

/* auto-generated wrapper for YHC.Primitive.primSignumWord32#X */
Node* Wrap_primSignumWord32(Node* node){
  Node* nResult = NULL;
  HsWord32 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primSignumWord32(UNBOX_WORD32(arg0));
  BOX_WORD32(nResult,pResult);
  return nResult;
}

HsWord32 primAbsWord32(HsWord32);

/* auto-generated wrapper for YHC.Primitive.primAbsWord32#X */
Node* Wrap_primAbsWord32(Node* node){
  Node* nResult = NULL;
  HsWord32 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAbsWord32(UNBOX_WORD32(arg0));
  BOX_WORD32(nResult,pResult);
  return nResult;
}

HsWord32 primMulWord32(HsWord32,HsWord32);

/* auto-generated wrapper for YHC.Primitive.primMulWord32#X */
Node* Wrap_primMulWord32(Node* node){
  Node* nResult = NULL;
  HsWord32 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primMulWord32(UNBOX_WORD32(arg0),UNBOX_WORD32(arg1));
  BOX_WORD32(nResult,pResult);
  return nResult;
}

HsWord32 primSubWord32(HsWord32,HsWord32);

/* auto-generated wrapper for YHC.Primitive.primSubWord32#X */
Node* Wrap_primSubWord32(Node* node){
  Node* nResult = NULL;
  HsWord32 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primSubWord32(UNBOX_WORD32(arg0),UNBOX_WORD32(arg1));
  BOX_WORD32(nResult,pResult);
  return nResult;
}

HsWord32 primAddWord32(HsWord32,HsWord32);

/* auto-generated wrapper for YHC.Primitive.primAddWord32#X */
Node* Wrap_primAddWord32(Node* node){
  Node* nResult = NULL;
  HsWord32 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primAddWord32(UNBOX_WORD32(arg0),UNBOX_WORD32(arg1));
  BOX_WORD32(nResult,pResult);
  return nResult;
}

HsBool primGeWord32(HsWord32,HsWord32);

/* auto-generated wrapper for YHC.Primitive.primGeWord32#X */
Node* Wrap_primGeWord32(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primGeWord32(UNBOX_WORD32(arg0),UNBOX_WORD32(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primGtWord32(HsWord32,HsWord32);

/* auto-generated wrapper for YHC.Primitive.primGtWord32#X */
Node* Wrap_primGtWord32(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primGtWord32(UNBOX_WORD32(arg0),UNBOX_WORD32(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primLeWord32(HsWord32,HsWord32);

/* auto-generated wrapper for YHC.Primitive.primLeWord32#X */
Node* Wrap_primLeWord32(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primLeWord32(UNBOX_WORD32(arg0),UNBOX_WORD32(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primLtWord32(HsWord32,HsWord32);

/* auto-generated wrapper for YHC.Primitive.primLtWord32#X */
Node* Wrap_primLtWord32(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primLtWord32(UNBOX_WORD32(arg0),UNBOX_WORD32(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primEqWord32(HsWord32,HsWord32);

/* auto-generated wrapper for YHC.Primitive.primEqWord32#X */
Node* Wrap_primEqWord32(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primEqWord32(UNBOX_WORD32(arg0),UNBOX_WORD32(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

Node* primWord16ToInteger(HsWord16);

/* auto-generated wrapper for YHC.Primitive.primWord16ToInteger#X */
Node* Wrap_primWord16ToInteger(Node* node){
  Node* nResult = NULL;
  Node* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primWord16ToInteger(UNBOX_WORD16(arg0));
  BOX_INTEGER(nResult,pResult);
  return nResult;
}

HsWord16 primWord16FromInteger(Node*);

/* auto-generated wrapper for YHC.Primitive.primWord16FromInteger#X */
Node* Wrap_primWord16FromInteger(Node* node){
  Node* nResult = NULL;
  HsWord16 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primWord16FromInteger(UNBOX_INTEGER(arg0));
  BOX_WORD16(nResult,pResult);
  return nResult;
}

HsInt primFromEnumWord16(HsWord16);

/* auto-generated wrapper for YHC.Primitive.primFromEnumWord16#X */
Node* Wrap_primFromEnumWord16(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primFromEnumWord16(UNBOX_WORD16(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

HsWord16 primToEnumWord16(HsInt);

/* auto-generated wrapper for YHC.Primitive.primToEnumWord16#X */
Node* Wrap_primToEnumWord16(Node* node){
  Node* nResult = NULL;
  HsWord16 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primToEnumWord16(UNBOX_INT(arg0));
  BOX_WORD16(nResult,pResult);
  return nResult;
}

HsWord16 primRemWord16(HsWord16,HsWord16);

/* auto-generated wrapper for YHC.Primitive.primRemWord16#X */
Node* Wrap_primRemWord16(Node* node){
  Node* nResult = NULL;
  HsWord16 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primRemWord16(UNBOX_WORD16(arg0),UNBOX_WORD16(arg1));
  BOX_WORD16(nResult,pResult);
  return nResult;
}

HsWord16 primQuotWord16(HsWord16,HsWord16);

/* auto-generated wrapper for YHC.Primitive.primQuotWord16#X */
Node* Wrap_primQuotWord16(Node* node){
  Node* nResult = NULL;
  HsWord16 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primQuotWord16(UNBOX_WORD16(arg0),UNBOX_WORD16(arg1));
  BOX_WORD16(nResult,pResult);
  return nResult;
}

HsWord16 primSignumWord16(HsWord16);

/* auto-generated wrapper for YHC.Primitive.primSignumWord16#X */
Node* Wrap_primSignumWord16(Node* node){
  Node* nResult = NULL;
  HsWord16 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primSignumWord16(UNBOX_WORD16(arg0));
  BOX_WORD16(nResult,pResult);
  return nResult;
}

HsWord16 primAbsWord16(HsWord16);

/* auto-generated wrapper for YHC.Primitive.primAbsWord16#X */
Node* Wrap_primAbsWord16(Node* node){
  Node* nResult = NULL;
  HsWord16 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAbsWord16(UNBOX_WORD16(arg0));
  BOX_WORD16(nResult,pResult);
  return nResult;
}

HsWord16 primMulWord16(HsWord16,HsWord16);

/* auto-generated wrapper for YHC.Primitive.primMulWord16#X */
Node* Wrap_primMulWord16(Node* node){
  Node* nResult = NULL;
  HsWord16 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primMulWord16(UNBOX_WORD16(arg0),UNBOX_WORD16(arg1));
  BOX_WORD16(nResult,pResult);
  return nResult;
}

HsWord16 primSubWord16(HsWord16,HsWord16);

/* auto-generated wrapper for YHC.Primitive.primSubWord16#X */
Node* Wrap_primSubWord16(Node* node){
  Node* nResult = NULL;
  HsWord16 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primSubWord16(UNBOX_WORD16(arg0),UNBOX_WORD16(arg1));
  BOX_WORD16(nResult,pResult);
  return nResult;
}

HsWord16 primAddWord16(HsWord16,HsWord16);

/* auto-generated wrapper for YHC.Primitive.primAddWord16#X */
Node* Wrap_primAddWord16(Node* node){
  Node* nResult = NULL;
  HsWord16 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primAddWord16(UNBOX_WORD16(arg0),UNBOX_WORD16(arg1));
  BOX_WORD16(nResult,pResult);
  return nResult;
}

HsBool primGeWord16(HsWord16,HsWord16);

/* auto-generated wrapper for YHC.Primitive.primGeWord16#X */
Node* Wrap_primGeWord16(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primGeWord16(UNBOX_WORD16(arg0),UNBOX_WORD16(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primGtWord16(HsWord16,HsWord16);

/* auto-generated wrapper for YHC.Primitive.primGtWord16#X */
Node* Wrap_primGtWord16(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primGtWord16(UNBOX_WORD16(arg0),UNBOX_WORD16(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primLeWord16(HsWord16,HsWord16);

/* auto-generated wrapper for YHC.Primitive.primLeWord16#X */
Node* Wrap_primLeWord16(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primLeWord16(UNBOX_WORD16(arg0),UNBOX_WORD16(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primLtWord16(HsWord16,HsWord16);

/* auto-generated wrapper for YHC.Primitive.primLtWord16#X */
Node* Wrap_primLtWord16(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primLtWord16(UNBOX_WORD16(arg0),UNBOX_WORD16(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primEqWord16(HsWord16,HsWord16);

/* auto-generated wrapper for YHC.Primitive.primEqWord16#X */
Node* Wrap_primEqWord16(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primEqWord16(UNBOX_WORD16(arg0),UNBOX_WORD16(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

Node* primWord8ToInteger(HsWord8);

/* auto-generated wrapper for YHC.Primitive.primWord8ToInteger#X */
Node* Wrap_primWord8ToInteger(Node* node){
  Node* nResult = NULL;
  Node* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primWord8ToInteger(UNBOX_WORD8(arg0));
  BOX_INTEGER(nResult,pResult);
  return nResult;
}

HsWord8 primWord8FromInteger(Node*);

/* auto-generated wrapper for YHC.Primitive.primWord8FromInteger#X */
Node* Wrap_primWord8FromInteger(Node* node){
  Node* nResult = NULL;
  HsWord8 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primWord8FromInteger(UNBOX_INTEGER(arg0));
  BOX_WORD8(nResult,pResult);
  return nResult;
}

HsInt primFromEnumWord8(HsWord8);

/* auto-generated wrapper for YHC.Primitive.primFromEnumWord8#X */
Node* Wrap_primFromEnumWord8(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primFromEnumWord8(UNBOX_WORD8(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

HsWord8 primToEnumWord8(HsInt);

/* auto-generated wrapper for YHC.Primitive.primToEnumWord8#X */
Node* Wrap_primToEnumWord8(Node* node){
  Node* nResult = NULL;
  HsWord8 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primToEnumWord8(UNBOX_INT(arg0));
  BOX_WORD8(nResult,pResult);
  return nResult;
}

HsWord8 primRemWord8(HsWord8,HsWord8);

/* auto-generated wrapper for YHC.Primitive.primRemWord8#X */
Node* Wrap_primRemWord8(Node* node){
  Node* nResult = NULL;
  HsWord8 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primRemWord8(UNBOX_WORD8(arg0),UNBOX_WORD8(arg1));
  BOX_WORD8(nResult,pResult);
  return nResult;
}

HsWord8 primQuotWord8(HsWord8,HsWord8);

/* auto-generated wrapper for YHC.Primitive.primQuotWord8#X */
Node* Wrap_primQuotWord8(Node* node){
  Node* nResult = NULL;
  HsWord8 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primQuotWord8(UNBOX_WORD8(arg0),UNBOX_WORD8(arg1));
  BOX_WORD8(nResult,pResult);
  return nResult;
}

HsWord8 primSignumWord8(HsWord8);

/* auto-generated wrapper for YHC.Primitive.primSignumWord8#X */
Node* Wrap_primSignumWord8(Node* node){
  Node* nResult = NULL;
  HsWord8 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primSignumWord8(UNBOX_WORD8(arg0));
  BOX_WORD8(nResult,pResult);
  return nResult;
}

HsWord8 primAbsWord8(HsWord8);

/* auto-generated wrapper for YHC.Primitive.primAbsWord8#X */
Node* Wrap_primAbsWord8(Node* node){
  Node* nResult = NULL;
  HsWord8 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAbsWord8(UNBOX_WORD8(arg0));
  BOX_WORD8(nResult,pResult);
  return nResult;
}

HsWord8 primMulWord8(HsWord8,HsWord8);

/* auto-generated wrapper for YHC.Primitive.primMulWord8#X */
Node* Wrap_primMulWord8(Node* node){
  Node* nResult = NULL;
  HsWord8 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primMulWord8(UNBOX_WORD8(arg0),UNBOX_WORD8(arg1));
  BOX_WORD8(nResult,pResult);
  return nResult;
}

HsWord8 primSubWord8(HsWord8,HsWord8);

/* auto-generated wrapper for YHC.Primitive.primSubWord8#X */
Node* Wrap_primSubWord8(Node* node){
  Node* nResult = NULL;
  HsWord8 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primSubWord8(UNBOX_WORD8(arg0),UNBOX_WORD8(arg1));
  BOX_WORD8(nResult,pResult);
  return nResult;
}

HsWord8 primAddWord8(HsWord8,HsWord8);

/* auto-generated wrapper for YHC.Primitive.primAddWord8#X */
Node* Wrap_primAddWord8(Node* node){
  Node* nResult = NULL;
  HsWord8 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primAddWord8(UNBOX_WORD8(arg0),UNBOX_WORD8(arg1));
  BOX_WORD8(nResult,pResult);
  return nResult;
}

HsBool primGeWord8(HsWord8,HsWord8);

/* auto-generated wrapper for YHC.Primitive.primGeWord8#X */
Node* Wrap_primGeWord8(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primGeWord8(UNBOX_WORD8(arg0),UNBOX_WORD8(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primGtWord8(HsWord8,HsWord8);

/* auto-generated wrapper for YHC.Primitive.primGtWord8#X */
Node* Wrap_primGtWord8(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primGtWord8(UNBOX_WORD8(arg0),UNBOX_WORD8(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primLeWord8(HsWord8,HsWord8);

/* auto-generated wrapper for YHC.Primitive.primLeWord8#X */
Node* Wrap_primLeWord8(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primLeWord8(UNBOX_WORD8(arg0),UNBOX_WORD8(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primLtWord8(HsWord8,HsWord8);

/* auto-generated wrapper for YHC.Primitive.primLtWord8#X */
Node* Wrap_primLtWord8(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primLtWord8(UNBOX_WORD8(arg0),UNBOX_WORD8(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primEqWord8(HsWord8,HsWord8);

/* auto-generated wrapper for YHC.Primitive.primEqWord8#X */
Node* Wrap_primEqWord8(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primEqWord8(UNBOX_WORD8(arg0),UNBOX_WORD8(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

Node* primInt64ToInteger(HsInt64);

/* auto-generated wrapper for YHC.Primitive.primInt64ToInteger#X */
Node* Wrap_primInt64ToInteger(Node* node){
  Node* nResult = NULL;
  Node* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primInt64ToInteger(UNBOX_INT64(arg0));
  BOX_INTEGER(nResult,pResult);
  return nResult;
}

HsInt64 primInt64FromInteger(Node*);

/* auto-generated wrapper for YHC.Primitive.primInt64FromInteger#X */
Node* Wrap_primInt64FromInteger(Node* node){
  Node* nResult = NULL;
  HsInt64 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primInt64FromInteger(UNBOX_INTEGER(arg0));
  BOX_INT64(nResult,pResult);
  return nResult;
}

HsInt primFromEnumInt64(HsInt64);

/* auto-generated wrapper for YHC.Primitive.primFromEnumInt64#X */
Node* Wrap_primFromEnumInt64(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primFromEnumInt64(UNBOX_INT64(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

HsInt64 primToEnumInt64(HsInt);

/* auto-generated wrapper for YHC.Primitive.primToEnumInt64#X */
Node* Wrap_primToEnumInt64(Node* node){
  Node* nResult = NULL;
  HsInt64 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primToEnumInt64(UNBOX_INT(arg0));
  BOX_INT64(nResult,pResult);
  return nResult;
}

HsInt64 primRemInt64(HsInt64,HsInt64);

/* auto-generated wrapper for YHC.Primitive.primRemInt64#X */
Node* Wrap_primRemInt64(Node* node){
  Node* nResult = NULL;
  HsInt64 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primRemInt64(UNBOX_INT64(arg0),UNBOX_INT64(arg1));
  BOX_INT64(nResult,pResult);
  return nResult;
}

HsInt64 primQuotInt64(HsInt64,HsInt64);

/* auto-generated wrapper for YHC.Primitive.primQuotInt64#X */
Node* Wrap_primQuotInt64(Node* node){
  Node* nResult = NULL;
  HsInt64 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primQuotInt64(UNBOX_INT64(arg0),UNBOX_INT64(arg1));
  BOX_INT64(nResult,pResult);
  return nResult;
}

HsInt64 primSignumInt64(HsInt64);

/* auto-generated wrapper for YHC.Primitive.primSignumInt64#X */
Node* Wrap_primSignumInt64(Node* node){
  Node* nResult = NULL;
  HsInt64 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primSignumInt64(UNBOX_INT64(arg0));
  BOX_INT64(nResult,pResult);
  return nResult;
}

HsInt64 primAbsInt64(HsInt64);

/* auto-generated wrapper for YHC.Primitive.primAbsInt64#X */
Node* Wrap_primAbsInt64(Node* node){
  Node* nResult = NULL;
  HsInt64 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAbsInt64(UNBOX_INT64(arg0));
  BOX_INT64(nResult,pResult);
  return nResult;
}

HsInt64 primMulInt64(HsInt64,HsInt64);

/* auto-generated wrapper for YHC.Primitive.primMulInt64#X */
Node* Wrap_primMulInt64(Node* node){
  Node* nResult = NULL;
  HsInt64 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primMulInt64(UNBOX_INT64(arg0),UNBOX_INT64(arg1));
  BOX_INT64(nResult,pResult);
  return nResult;
}

HsInt64 primSubInt64(HsInt64,HsInt64);

/* auto-generated wrapper for YHC.Primitive.primSubInt64#X */
Node* Wrap_primSubInt64(Node* node){
  Node* nResult = NULL;
  HsInt64 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primSubInt64(UNBOX_INT64(arg0),UNBOX_INT64(arg1));
  BOX_INT64(nResult,pResult);
  return nResult;
}

HsInt64 primAddInt64(HsInt64,HsInt64);

/* auto-generated wrapper for YHC.Primitive.primAddInt64#X */
Node* Wrap_primAddInt64(Node* node){
  Node* nResult = NULL;
  HsInt64 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primAddInt64(UNBOX_INT64(arg0),UNBOX_INT64(arg1));
  BOX_INT64(nResult,pResult);
  return nResult;
}

HsBool primGeInt64(HsInt64,HsInt64);

/* auto-generated wrapper for YHC.Primitive.primGeInt64#X */
Node* Wrap_primGeInt64(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primGeInt64(UNBOX_INT64(arg0),UNBOX_INT64(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primGtInt64(HsInt64,HsInt64);

/* auto-generated wrapper for YHC.Primitive.primGtInt64#X */
Node* Wrap_primGtInt64(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primGtInt64(UNBOX_INT64(arg0),UNBOX_INT64(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primLeInt64(HsInt64,HsInt64);

/* auto-generated wrapper for YHC.Primitive.primLeInt64#X */
Node* Wrap_primLeInt64(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primLeInt64(UNBOX_INT64(arg0),UNBOX_INT64(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primLtInt64(HsInt64,HsInt64);

/* auto-generated wrapper for YHC.Primitive.primLtInt64#X */
Node* Wrap_primLtInt64(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primLtInt64(UNBOX_INT64(arg0),UNBOX_INT64(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primEqInt64(HsInt64,HsInt64);

/* auto-generated wrapper for YHC.Primitive.primEqInt64#X */
Node* Wrap_primEqInt64(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primEqInt64(UNBOX_INT64(arg0),UNBOX_INT64(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

Node* primInt32ToInteger(HsInt32);

/* auto-generated wrapper for YHC.Primitive.primInt32ToInteger#X */
Node* Wrap_primInt32ToInteger(Node* node){
  Node* nResult = NULL;
  Node* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primInt32ToInteger(UNBOX_INT32(arg0));
  BOX_INTEGER(nResult,pResult);
  return nResult;
}

HsInt32 primInt32FromInteger(Node*);

/* auto-generated wrapper for YHC.Primitive.primInt32FromInteger#X */
Node* Wrap_primInt32FromInteger(Node* node){
  Node* nResult = NULL;
  HsInt32 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primInt32FromInteger(UNBOX_INTEGER(arg0));
  BOX_INT32(nResult,pResult);
  return nResult;
}

HsInt primFromEnumInt32(HsInt32);

/* auto-generated wrapper for YHC.Primitive.primFromEnumInt32#X */
Node* Wrap_primFromEnumInt32(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primFromEnumInt32(UNBOX_INT32(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

HsInt32 primToEnumInt32(HsInt);

/* auto-generated wrapper for YHC.Primitive.primToEnumInt32#X */
Node* Wrap_primToEnumInt32(Node* node){
  Node* nResult = NULL;
  HsInt32 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primToEnumInt32(UNBOX_INT(arg0));
  BOX_INT32(nResult,pResult);
  return nResult;
}

HsInt32 primRemInt32(HsInt32,HsInt32);

/* auto-generated wrapper for YHC.Primitive.primRemInt32#X */
Node* Wrap_primRemInt32(Node* node){
  Node* nResult = NULL;
  HsInt32 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primRemInt32(UNBOX_INT32(arg0),UNBOX_INT32(arg1));
  BOX_INT32(nResult,pResult);
  return nResult;
}

HsInt32 primQuotInt32(HsInt32,HsInt32);

/* auto-generated wrapper for YHC.Primitive.primQuotInt32#X */
Node* Wrap_primQuotInt32(Node* node){
  Node* nResult = NULL;
  HsInt32 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primQuotInt32(UNBOX_INT32(arg0),UNBOX_INT32(arg1));
  BOX_INT32(nResult,pResult);
  return nResult;
}

HsInt32 primSignumInt32(HsInt32);

/* auto-generated wrapper for YHC.Primitive.primSignumInt32#X */
Node* Wrap_primSignumInt32(Node* node){
  Node* nResult = NULL;
  HsInt32 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primSignumInt32(UNBOX_INT32(arg0));
  BOX_INT32(nResult,pResult);
  return nResult;
}

HsInt32 primAbsInt32(HsInt32);

/* auto-generated wrapper for YHC.Primitive.primAbsInt32#X */
Node* Wrap_primAbsInt32(Node* node){
  Node* nResult = NULL;
  HsInt32 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAbsInt32(UNBOX_INT32(arg0));
  BOX_INT32(nResult,pResult);
  return nResult;
}

HsInt32 primMulInt32(HsInt32,HsInt32);

/* auto-generated wrapper for YHC.Primitive.primMulInt32#X */
Node* Wrap_primMulInt32(Node* node){
  Node* nResult = NULL;
  HsInt32 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primMulInt32(UNBOX_INT32(arg0),UNBOX_INT32(arg1));
  BOX_INT32(nResult,pResult);
  return nResult;
}

HsInt32 primSubInt32(HsInt32,HsInt32);

/* auto-generated wrapper for YHC.Primitive.primSubInt32#X */
Node* Wrap_primSubInt32(Node* node){
  Node* nResult = NULL;
  HsInt32 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primSubInt32(UNBOX_INT32(arg0),UNBOX_INT32(arg1));
  BOX_INT32(nResult,pResult);
  return nResult;
}

HsInt32 primAddInt32(HsInt32,HsInt32);

/* auto-generated wrapper for YHC.Primitive.primAddInt32#X */
Node* Wrap_primAddInt32(Node* node){
  Node* nResult = NULL;
  HsInt32 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primAddInt32(UNBOX_INT32(arg0),UNBOX_INT32(arg1));
  BOX_INT32(nResult,pResult);
  return nResult;
}

HsBool primGeInt32(HsInt32,HsInt32);

/* auto-generated wrapper for YHC.Primitive.primGeInt32#X */
Node* Wrap_primGeInt32(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primGeInt32(UNBOX_INT32(arg0),UNBOX_INT32(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primGtInt32(HsInt32,HsInt32);

/* auto-generated wrapper for YHC.Primitive.primGtInt32#X */
Node* Wrap_primGtInt32(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primGtInt32(UNBOX_INT32(arg0),UNBOX_INT32(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primLeInt32(HsInt32,HsInt32);

/* auto-generated wrapper for YHC.Primitive.primLeInt32#X */
Node* Wrap_primLeInt32(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primLeInt32(UNBOX_INT32(arg0),UNBOX_INT32(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primLtInt32(HsInt32,HsInt32);

/* auto-generated wrapper for YHC.Primitive.primLtInt32#X */
Node* Wrap_primLtInt32(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primLtInt32(UNBOX_INT32(arg0),UNBOX_INT32(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primEqInt32(HsInt32,HsInt32);

/* auto-generated wrapper for YHC.Primitive.primEqInt32#X */
Node* Wrap_primEqInt32(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primEqInt32(UNBOX_INT32(arg0),UNBOX_INT32(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

Node* primInt16ToInteger(HsInt8);

/* auto-generated wrapper for YHC.Primitive.primInt16ToInteger#X */
Node* Wrap_primInt16ToInteger(Node* node){
  Node* nResult = NULL;
  Node* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primInt16ToInteger(UNBOX_INT8(arg0));
  BOX_INTEGER(nResult,pResult);
  return nResult;
}

HsInt8 primInt16FromInteger(Node*);

/* auto-generated wrapper for YHC.Primitive.primInt16FromInteger#X */
Node* Wrap_primInt16FromInteger(Node* node){
  Node* nResult = NULL;
  HsInt8 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primInt16FromInteger(UNBOX_INTEGER(arg0));
  BOX_INT8(nResult,pResult);
  return nResult;
}

HsInt primFromEnumInt16(HsInt8);

/* auto-generated wrapper for YHC.Primitive.primFromEnumInt16#X */
Node* Wrap_primFromEnumInt16(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primFromEnumInt16(UNBOX_INT8(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

HsInt8 primToEnumInt16(HsInt);

/* auto-generated wrapper for YHC.Primitive.primToEnumInt16#X */
Node* Wrap_primToEnumInt16(Node* node){
  Node* nResult = NULL;
  HsInt8 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primToEnumInt16(UNBOX_INT(arg0));
  BOX_INT8(nResult,pResult);
  return nResult;
}

HsInt8 primRemInt16(HsInt8,HsInt8);

/* auto-generated wrapper for YHC.Primitive.primRemInt16#X */
Node* Wrap_primRemInt16(Node* node){
  Node* nResult = NULL;
  HsInt8 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primRemInt16(UNBOX_INT8(arg0),UNBOX_INT8(arg1));
  BOX_INT8(nResult,pResult);
  return nResult;
}

HsInt8 primQuotInt16(HsInt8,HsInt8);

/* auto-generated wrapper for YHC.Primitive.primQuotInt16#X */
Node* Wrap_primQuotInt16(Node* node){
  Node* nResult = NULL;
  HsInt8 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primQuotInt16(UNBOX_INT8(arg0),UNBOX_INT8(arg1));
  BOX_INT8(nResult,pResult);
  return nResult;
}

HsInt8 primSignumInt16(HsInt8);

/* auto-generated wrapper for YHC.Primitive.primSignumInt16#X */
Node* Wrap_primSignumInt16(Node* node){
  Node* nResult = NULL;
  HsInt8 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primSignumInt16(UNBOX_INT8(arg0));
  BOX_INT8(nResult,pResult);
  return nResult;
}

HsInt8 primAbsInt16(HsInt8);

/* auto-generated wrapper for YHC.Primitive.primAbsInt16#X */
Node* Wrap_primAbsInt16(Node* node){
  Node* nResult = NULL;
  HsInt8 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAbsInt16(UNBOX_INT8(arg0));
  BOX_INT8(nResult,pResult);
  return nResult;
}

HsInt8 primMulInt16(HsInt8,HsInt8);

/* auto-generated wrapper for YHC.Primitive.primMulInt16#X */
Node* Wrap_primMulInt16(Node* node){
  Node* nResult = NULL;
  HsInt8 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primMulInt16(UNBOX_INT8(arg0),UNBOX_INT8(arg1));
  BOX_INT8(nResult,pResult);
  return nResult;
}

HsInt8 primSubInt16(HsInt8,HsInt8);

/* auto-generated wrapper for YHC.Primitive.primSubInt16#X */
Node* Wrap_primSubInt16(Node* node){
  Node* nResult = NULL;
  HsInt8 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primSubInt16(UNBOX_INT8(arg0),UNBOX_INT8(arg1));
  BOX_INT8(nResult,pResult);
  return nResult;
}

HsInt8 primAddInt16(HsInt8,HsInt8);

/* auto-generated wrapper for YHC.Primitive.primAddInt16#X */
Node* Wrap_primAddInt16(Node* node){
  Node* nResult = NULL;
  HsInt8 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primAddInt16(UNBOX_INT8(arg0),UNBOX_INT8(arg1));
  BOX_INT8(nResult,pResult);
  return nResult;
}

HsBool primGeInt16(HsInt8,HsInt8);

/* auto-generated wrapper for YHC.Primitive.primGeInt16#X */
Node* Wrap_primGeInt16(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primGeInt16(UNBOX_INT8(arg0),UNBOX_INT8(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primGtInt16(HsInt8,HsInt8);

/* auto-generated wrapper for YHC.Primitive.primGtInt16#X */
Node* Wrap_primGtInt16(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primGtInt16(UNBOX_INT8(arg0),UNBOX_INT8(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primLeInt16(HsInt8,HsInt8);

/* auto-generated wrapper for YHC.Primitive.primLeInt16#X */
Node* Wrap_primLeInt16(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primLeInt16(UNBOX_INT8(arg0),UNBOX_INT8(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primLtInt16(HsInt8,HsInt8);

/* auto-generated wrapper for YHC.Primitive.primLtInt16#X */
Node* Wrap_primLtInt16(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primLtInt16(UNBOX_INT8(arg0),UNBOX_INT8(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primEqInt16(HsInt8,HsInt8);

/* auto-generated wrapper for YHC.Primitive.primEqInt16#X */
Node* Wrap_primEqInt16(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primEqInt16(UNBOX_INT8(arg0),UNBOX_INT8(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

Node* primInt8ToInteger(HsInt8);

/* auto-generated wrapper for YHC.Primitive.primInt8ToInteger#X */
Node* Wrap_primInt8ToInteger(Node* node){
  Node* nResult = NULL;
  Node* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primInt8ToInteger(UNBOX_INT8(arg0));
  BOX_INTEGER(nResult,pResult);
  return nResult;
}

HsInt8 primInt8FromInteger(Node*);

/* auto-generated wrapper for YHC.Primitive.primInt8FromInteger#X */
Node* Wrap_primInt8FromInteger(Node* node){
  Node* nResult = NULL;
  HsInt8 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primInt8FromInteger(UNBOX_INTEGER(arg0));
  BOX_INT8(nResult,pResult);
  return nResult;
}

HsInt primFromEnumInt8(HsInt8);

/* auto-generated wrapper for YHC.Primitive.primFromEnumInt8#X */
Node* Wrap_primFromEnumInt8(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primFromEnumInt8(UNBOX_INT8(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

HsInt8 primToEnumInt8(HsInt);

/* auto-generated wrapper for YHC.Primitive.primToEnumInt8#X */
Node* Wrap_primToEnumInt8(Node* node){
  Node* nResult = NULL;
  HsInt8 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primToEnumInt8(UNBOX_INT(arg0));
  BOX_INT8(nResult,pResult);
  return nResult;
}

HsInt8 primRemInt8(HsInt8,HsInt8);

/* auto-generated wrapper for YHC.Primitive.primRemInt8#X */
Node* Wrap_primRemInt8(Node* node){
  Node* nResult = NULL;
  HsInt8 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primRemInt8(UNBOX_INT8(arg0),UNBOX_INT8(arg1));
  BOX_INT8(nResult,pResult);
  return nResult;
}

HsInt8 primQuotInt8(HsInt8,HsInt8);

/* auto-generated wrapper for YHC.Primitive.primQuotInt8#X */
Node* Wrap_primQuotInt8(Node* node){
  Node* nResult = NULL;
  HsInt8 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primQuotInt8(UNBOX_INT8(arg0),UNBOX_INT8(arg1));
  BOX_INT8(nResult,pResult);
  return nResult;
}

HsInt8 primSignumInt8(HsInt8);

/* auto-generated wrapper for YHC.Primitive.primSignumInt8#X */
Node* Wrap_primSignumInt8(Node* node){
  Node* nResult = NULL;
  HsInt8 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primSignumInt8(UNBOX_INT8(arg0));
  BOX_INT8(nResult,pResult);
  return nResult;
}

HsInt8 primAbsInt8(HsInt8);

/* auto-generated wrapper for YHC.Primitive.primAbsInt8#X */
Node* Wrap_primAbsInt8(Node* node){
  Node* nResult = NULL;
  HsInt8 pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primAbsInt8(UNBOX_INT8(arg0));
  BOX_INT8(nResult,pResult);
  return nResult;
}

HsInt8 primMulInt8(HsInt8,HsInt8);

/* auto-generated wrapper for YHC.Primitive.primMulInt8#X */
Node* Wrap_primMulInt8(Node* node){
  Node* nResult = NULL;
  HsInt8 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primMulInt8(UNBOX_INT8(arg0),UNBOX_INT8(arg1));
  BOX_INT8(nResult,pResult);
  return nResult;
}

HsInt8 primSubInt8(HsInt8,HsInt8);

/* auto-generated wrapper for YHC.Primitive.primSubInt8#X */
Node* Wrap_primSubInt8(Node* node){
  Node* nResult = NULL;
  HsInt8 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primSubInt8(UNBOX_INT8(arg0),UNBOX_INT8(arg1));
  BOX_INT8(nResult,pResult);
  return nResult;
}

HsInt8 primAddInt8(HsInt8,HsInt8);

/* auto-generated wrapper for YHC.Primitive.primAddInt8#X */
Node* Wrap_primAddInt8(Node* node){
  Node* nResult = NULL;
  HsInt8 pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primAddInt8(UNBOX_INT8(arg0),UNBOX_INT8(arg1));
  BOX_INT8(nResult,pResult);
  return nResult;
}

HsBool primGeInt8(HsInt8,HsInt8);

/* auto-generated wrapper for YHC.Primitive.primGeInt8#X */
Node* Wrap_primGeInt8(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primGeInt8(UNBOX_INT8(arg0),UNBOX_INT8(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primGtInt8(HsInt8,HsInt8);

/* auto-generated wrapper for YHC.Primitive.primGtInt8#X */
Node* Wrap_primGtInt8(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primGtInt8(UNBOX_INT8(arg0),UNBOX_INT8(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primLeInt8(HsInt8,HsInt8);

/* auto-generated wrapper for YHC.Primitive.primLeInt8#X */
Node* Wrap_primLeInt8(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primLeInt8(UNBOX_INT8(arg0),UNBOX_INT8(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primLtInt8(HsInt8,HsInt8);

/* auto-generated wrapper for YHC.Primitive.primLtInt8#X */
Node* Wrap_primLtInt8(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primLtInt8(UNBOX_INT8(arg0),UNBOX_INT8(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primEqInt8(HsInt8,HsInt8);

/* auto-generated wrapper for YHC.Primitive.primEqInt8#X */
Node* Wrap_primEqInt8(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primEqInt8(UNBOX_INT8(arg0),UNBOX_INT8(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

void primMemmove(void*,void*,HsInt);

/* auto-generated wrapper for YHC.Primitive.primMemmove##X */
Node* Wrap_primMemmove(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  Node* arg2 = node->args[2];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  REMOVE_IND(arg2, Node*);
  primMemmove(UNBOX_PTR(arg0),UNBOX_PTR(arg1),UNBOX_INT(arg2));
  nResult = NODE_UNIT;
  return nResult;
}

void primMemcpy(void*,void*,HsInt);

/* auto-generated wrapper for YHC.Primitive.primMemcpy##X */
Node* Wrap_primMemcpy(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  Node* arg2 = node->args[2];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  REMOVE_IND(arg2, Node*);
  primMemcpy(UNBOX_PTR(arg0),UNBOX_PTR(arg1),UNBOX_INT(arg2));
  nResult = NODE_UNIT;
  return nResult;
}

FunPtr primFinalizerFree(void);

/* auto-generated wrapper for YHC.Primitive.primFinalizerFree#X */
Node* Wrap_primFinalizerFree(Node* node){
  Node* nResult = NULL;
  FunPtr pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primFinalizerFree(UNBOX_UNIT(arg0));
  BOX_FUN_PTR(nResult,pResult);
  return nResult;
}

void* primRealloc(void*,HsInt);

/* auto-generated wrapper for YHC.Primitive.primRealloc##X */
Node* Wrap_primRealloc(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primRealloc(UNBOX_PTR(arg0),UNBOX_INT(arg1));
  BOX_PTR(nResult,pResult);
  return nResult;
}

void primFree(void*);

/* auto-generated wrapper for YHC.Primitive.primFree##X */
Node* Wrap_primFree(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  primFree(UNBOX_PTR(arg0));
  nResult = NODE_UNIT;
  return nResult;
}

void* primMalloc(HsInt);

/* auto-generated wrapper for YHC.Primitive.primMalloc##X */
Node* Wrap_primMalloc(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primMalloc(UNBOX_INT(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

void primFreeCString(void*);

/* auto-generated wrapper for YHC.Primitive.primFreeCString##X */
Node* Wrap_primFreeCString(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  primFreeCString(UNBOX_PTR(arg0));
  nResult = NODE_UNIT;
  return nResult;
}

Node* primNewCString(Node* node);

HsInt getErrorNo(void);

/* auto-generated wrapper for YHC.Primitive.getErrorNo##X */
Node* Wrap_getErrorNo(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = getErrorNo(UNBOX_UNIT(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

Node* primDerefStablePtr(void*);

/* auto-generated wrapper for YHC.Primitive.primDerefStablePtr##X */
Node* Wrap_primDerefStablePtr(Node* node){
  Node* nResult = NULL;
  Node* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primDerefStablePtr(UNBOX_PTR(arg0));
  BOX_UNKNOWN(nResult,pResult);
  return nResult;
}

void primFreeStablePtr(void*);

/* auto-generated wrapper for YHC.Primitive.primFreeStablePtr##X */
Node* Wrap_primFreeStablePtr(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  primFreeStablePtr(UNBOX_PTR(arg0));
  nResult = NODE_UNIT;
  return nResult;
}

void* primCreateStablePtr(Node*);

/* auto-generated wrapper for YHC.Primitive.primCreateStablePtr##X */
Node* Wrap_primCreateStablePtr(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primCreateStablePtr(UNBOX_UNKNOWN(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

Node* primFreeForeignPtr(Node* node);

Node* primDerefForeignPtr(Node* node);

Node* primAttachForeignPtr(Node* node);

Node* primCreateForeignPtr(Node* node);

/* auto-generated wrapper for YHC.Primitive.castPtrToFunPtr#X */
Node* Wrap_castPtrToFunPtr(Node* node){
  Node* nResult = NULL;
  FunPtr pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = (FunPtr)(UNBOX_PTR(arg0));
  BOX_FUN_PTR(nResult,pResult);
  return nResult;
}

/* auto-generated wrapper for YHC.Primitive.castFunPtrToPtr#X */
Node* Wrap_castFunPtrToPtr(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = (void*)(UNBOX_FUN_PTR(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

/* auto-generated wrapper for YHC.Primitive.funPtrToInt#X */
Node* Wrap_funPtrToInt(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = (HsInt)(UNBOX_FUN_PTR(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

/* auto-generated wrapper for YHC.Primitive.castPtr#X */
Node* Wrap_castPtr(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = (void*)(UNBOX_PTR(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

/* auto-generated wrapper for YHC.Primitive.intToPtr#X */
Node* Wrap_intToPtr(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = (void*)(UNBOX_INT(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

/* auto-generated wrapper for YHC.Primitive.ptrToInt#X */
Node* Wrap_ptrToInt(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = (HsInt)(UNBOX_PTR(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

Node* primTryTakeMVar(Node* node);

Node* primIsEmptyMVar(Node* node);

Node* primSwapMVar(Node* node);

Node* primReadMVar(Node* node);

Node* primTakeMVar(Node* node);

Node* primPutMVar(Node* node);

Node* primNewMVar(Node* node);

Node* primMyThreadId(Node* node);

Node* primSpinLockProcess(Node* node);

Node* primKillProcess(Node* node);

Node* primSpawnProcess(Node* node);

void primUnblockExceptions(void);

/* auto-generated wrapper for YHC.Primitive.primUnblockExceptions##X */
Node* Wrap_primUnblockExceptions(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  primUnblockExceptions(UNBOX_UNIT(arg0));
  nResult = NODE_UNIT;
  return nResult;
}

void primBlockExceptions(void);

/* auto-generated wrapper for YHC.Primitive.primBlockExceptions##X */
Node* Wrap_primBlockExceptions(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  primBlockExceptions(UNBOX_UNIT(arg0));
  nResult = NODE_UNIT;
  return nResult;
}

HsBool primDirExists(void*);

/* auto-generated wrapper for YHC.Primitive.primDirExists##X */
Node* Wrap_primDirExists(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primDirExists(UNBOX_PTR(arg0));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsBool primFileExists(void*);

/* auto-generated wrapper for YHC.Primitive.primFileExists##X */
Node* Wrap_primFileExists(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primFileExists(UNBOX_PTR(arg0));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsInt primCopyFile(void*,void*);

/* auto-generated wrapper for YHC.Primitive.primCopyFile##X */
Node* Wrap_primCopyFile(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primCopyFile(UNBOX_PTR(arg0),UNBOX_PTR(arg1));
  BOX_INT(nResult,pResult);
  return nResult;
}

HsInt primRenameFile(void*,void*);

/* auto-generated wrapper for YHC.Primitive.primRenameFile##X */
Node* Wrap_primRenameFile(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primRenameFile(UNBOX_PTR(arg0),UNBOX_PTR(arg1));
  BOX_INT(nResult,pResult);
  return nResult;
}

HsInt primRemoveFile(void*);

/* auto-generated wrapper for YHC.Primitive.primRemoveFile##X */
Node* Wrap_primRemoveFile(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primRemoveFile(UNBOX_PTR(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

void* primGetHomeDir(void);

/* auto-generated wrapper for YHC.Primitive.primGetHomeDir##X */
Node* Wrap_primGetHomeDir(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primGetHomeDir(UNBOX_UNIT(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

HsInt primSetCurrentDir(void*);

/* auto-generated wrapper for YHC.Primitive.primSetCurrentDir##X */
Node* Wrap_primSetCurrentDir(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primSetCurrentDir(UNBOX_PTR(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

void* primGetCurrentDir(void);

/* auto-generated wrapper for YHC.Primitive.primGetCurrentDir##X */
Node* Wrap_primGetCurrentDir(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primGetCurrentDir(UNBOX_UNIT(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

HsInt primCloseDir(void*);

/* auto-generated wrapper for YHC.Primitive.primCloseDir##X */
Node* Wrap_primCloseDir(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primCloseDir(UNBOX_PTR(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

void* primNextDir(void*);

/* auto-generated wrapper for YHC.Primitive.primNextDir##X */
Node* Wrap_primNextDir(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primNextDir(UNBOX_PTR(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

void* primGetDirName(void*);

/* auto-generated wrapper for YHC.Primitive.primGetDirName##X */
Node* Wrap_primGetDirName(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primGetDirName(UNBOX_PTR(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

void* primOpenDir(void*);

/* auto-generated wrapper for YHC.Primitive.primOpenDir##X */
Node* Wrap_primOpenDir(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primOpenDir(UNBOX_PTR(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

HsInt primRenameDir(void*,void*);

/* auto-generated wrapper for YHC.Primitive.primRenameDir##X */
Node* Wrap_primRenameDir(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primRenameDir(UNBOX_PTR(arg0),UNBOX_PTR(arg1));
  BOX_INT(nResult,pResult);
  return nResult;
}

HsInt primRemoveDir(void*);

/* auto-generated wrapper for YHC.Primitive.primRemoveDir##X */
Node* Wrap_primRemoveDir(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primRemoveDir(UNBOX_PTR(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

HsInt primCreateDir(void*);

/* auto-generated wrapper for YHC.Primitive.primCreateDir##X */
Node* Wrap_primCreateDir(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primCreateDir(UNBOX_PTR(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

void* primGetEnv(void*);

/* auto-generated wrapper for YHC.Primitive.primGetEnv##X */
Node* Wrap_primGetEnv(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primGetEnv(UNBOX_PTR(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

void* primGetArg(HsInt);

/* auto-generated wrapper for YHC.Primitive.primGetArg##X */
Node* Wrap_primGetArg(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primGetArg(UNBOX_INT(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

void* primGetProgName(void);

/* auto-generated wrapper for YHC.Primitive.primGetProgName##X */
Node* Wrap_primGetProgName(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primGetProgName(UNBOX_UNIT(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

Node* primExitWith(HsInt);

/* auto-generated wrapper for YHC.Primitive.primExitWith#X */
Node* Wrap_primExitWith(Node* node){
  Node* nResult = NULL;
  Node* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = primExitWith(UNBOX_INT(arg0));
  BOX_UNKNOWN(nResult,pResult);
  return nResult;
}

void* stdinC(void);

/* auto-generated wrapper for YHC.Primitive.stdinC#X */
Node* Wrap_stdinC(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = stdinC(UNBOX_UNIT(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

void* stderrC(void);

/* auto-generated wrapper for YHC.Primitive.stderrC#X */
Node* Wrap_stderrC(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = stderrC(UNBOX_UNIT(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

void* stdoutC(void);

/* auto-generated wrapper for YHC.Primitive.stdoutC#X */
Node* Wrap_stdoutC(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = stdoutC(UNBOX_UNIT(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

void* openFileC(void*,HsInt);

/* auto-generated wrapper for YHC.Primitive.openFileC##X */
Node* Wrap_openFileC(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = openFileC(UNBOX_PTR(arg0),UNBOX_INT(arg1));
  BOX_PTR(nResult,pResult);
  return nResult;
}

HsInt hTellC(void*);

/* auto-generated wrapper for YHC.Primitive.hTellC##X */
Node* Wrap_hTellC(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = hTellC(UNBOX_FOREIGN_PTR(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

HsInt hGetBufferingC(void*);

/* auto-generated wrapper for YHC.Primitive.hGetBufferingC##X */
Node* Wrap_hGetBufferingC(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = hGetBufferingC(UNBOX_FOREIGN_PTR(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

void hSetBufferingC(void*,HsInt);

/* auto-generated wrapper for YHC.Primitive.hSetBufferingC##X */
Node* Wrap_hSetBufferingC(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  hSetBufferingC(UNBOX_FOREIGN_PTR(arg0),UNBOX_INT(arg1));
  nResult = NODE_UNIT;
  return nResult;
}

void hSeekC(void*,HsInt,HsInt);

/* auto-generated wrapper for YHC.Primitive.hSeekC##X */
Node* Wrap_hSeekC(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  Node* arg2 = node->args[2];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  REMOVE_IND(arg2, Node*);
  hSeekC(UNBOX_FOREIGN_PTR(arg0),UNBOX_INT(arg1),UNBOX_INT(arg2));
  nResult = NODE_UNIT;
  return nResult;
}

void hPutCharC(void*,char);

/* auto-generated wrapper for YHC.Primitive.hPutCharC##X */
Node* Wrap_hPutCharC(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  hPutCharC(UNBOX_FOREIGN_PTR(arg0),UNBOX_CHAR(arg1));
  nResult = NODE_UNIT;
  return nResult;
}

HsBool hIsEOFC(void*);

/* auto-generated wrapper for YHC.Primitive.hIsEOFC##X */
Node* Wrap_hIsEOFC(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = hIsEOFC(UNBOX_FOREIGN_PTR(arg0));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

void* hGetTypeC(void*);

/* auto-generated wrapper for YHC.Primitive.hGetTypeC##X */
Node* Wrap_hGetTypeC(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = hGetTypeC(UNBOX_FOREIGN_PTR(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

void* hGetFileNameC(void*);

/* auto-generated wrapper for YHC.Primitive.hGetFileNameC##X */
Node* Wrap_hGetFileNameC(Node* node){
  Node* nResult = NULL;
  void* pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = hGetFileNameC(UNBOX_FOREIGN_PTR(arg0));
  BOX_PTR(nResult,pResult);
  return nResult;
}

void hFlushC(void*);

/* auto-generated wrapper for YHC.Primitive.hFlushC##X */
Node* Wrap_hFlushC(Node* node){
  Node* nResult = NULL;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  hFlushC(UNBOX_FOREIGN_PTR(arg0));
  nResult = NODE_UNIT;
  return nResult;
}

HsInt hGetCharC(void*);

/* auto-generated wrapper for YHC.Primitive.hGetCharC##X */
Node* Wrap_hGetCharC(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = hGetCharC(UNBOX_FOREIGN_PTR(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

HsInt hFileSizeC(void*);

/* auto-generated wrapper for YHC.Primitive.hFileSizeC##X */
Node* Wrap_hFileSizeC(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = hFileSizeC(UNBOX_FOREIGN_PTR(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

FunPtr hCloseC(void*);

/* auto-generated wrapper for YHC.Primitive.hCloseC#X */
Node* Wrap_hCloseC(Node* node){
  Node* nResult = NULL;
  FunPtr pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = hCloseC(UNBOX_FOREIGN_PTR(arg0));
  BOX_FUN_PTR(nResult,pResult);
  return nResult;
}

HsBool primEqHandleC(void*,void*);

/* auto-generated wrapper for YHC.Primitive.primEqHandleC#X */
Node* Wrap_primEqHandleC(Node* node){
  Node* nResult = NULL;
  HsBool pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primEqHandleC(UNBOX_FOREIGN_PTR(arg0),UNBOX_FOREIGN_PTR(arg1));
  BOX_BOOL(nResult,pResult);
  return nResult;
}

HsInt hGetErrorC(void*);

/* auto-generated wrapper for YHC.Primitive.hGetErrorC##X */
Node* Wrap_hGetErrorC(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = hGetErrorC(UNBOX_FOREIGN_PTR(arg0));
  BOX_INT(nResult,pResult);
  return nResult;
}

Node* primUnsafeCoerce(Node* node);

Node* primStrError(Node* node);

Node* primIntegerRem(Node* node);

Node* primIntegerQuot(Node* node);

Node* primIntegerQuotRem(Node* node);

Node* primIntegerNeg(Node* node);

Node* primIntegerMul(Node* node);

Node* primIntegerSub(Node* node);

Node* primIntegerAdd(Node* node);

Node* primIntegerGt(Node* node);

Node* primIntegerGe(Node* node);

Node* primIntegerLe(Node* node);

Node* primIntegerLt(Node* node);

Node* primIntegerNe(Node* node);

Node* primIntegerEq(Node* node);

Node* primEncodeDouble(Node* node);

Node* primDecodeDouble(Node* node);

Node* primDoubleSignum(Node* node);

Node* primDoubleAbs(Node* node);

Node* primDoubleFromInteger(Node* node);

Node* primDoublePow(Node* node);

Node* primDoubleATan(Node* node);

Node* primDoubleACos(Node* node);

Node* primDoubleASin(Node* node);

Node* primDoubleTan(Node* node);

Node* primDoubleCos(Node* node);

Node* primDoubleSin(Node* node);

Node* primDoubleSqrt(Node* node);

Node* primDoubleLog(Node* node);

Node* primDoubleExp(Node* node);

Node* primEncodeFloat(Node* node);

Node* primDecodeFloat(Node* node);

Node* primFloatSignum(Node* node);

Node* primFloatAbs(Node* node);

Node* primFloatFromInteger(Node* node);

Node* primFloatPow(Node* node);

Node* primFloatATan(Node* node);

Node* primFloatACos(Node* node);

Node* primFloatASin(Node* node);

Node* primFloatTan(Node* node);

Node* primFloatCos(Node* node);

Node* primFloatSin(Node* node);

Node* primFloatSqrt(Node* node);

Node* primFloatLog(Node* node);

Node* primFloatExp(Node* node);

Node* primIntegerFromInt(Node* node);

Node* primIntFromInteger(Node* node);

Node* primIntSignum(Node* node);

Node* primIntAbs(Node* node);

HsInt primComparePS(char*,char*);

/* auto-generated wrapper for YHC.Primitive.primComparePS#X */
Node* Wrap_primComparePS(Node* node){
  Node* nResult = NULL;
  HsInt pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primComparePS(UNBOX_STRING(arg0),UNBOX_STRING(arg1));
  BOX_INT(nResult,pResult);
  return nResult;
}

char primPSGetChar(char*,HsInt);

/* auto-generated wrapper for YHC.Primitive.primPSGetChar#X */
Node* Wrap_primPSGetChar(Node* node){
  Node* nResult = NULL;
  char pResult;
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg1, Node*);
  pResult = primPSGetChar(UNBOX_STRING(arg0),UNBOX_INT(arg1));
  BOX_CHAR(nResult,pResult);
  return nResult;
}

Node* primPackString(Node* node);

Node* primWriteIORefC(Node* node);

Node* primReadIORefC(Node* node);

Node* primNewIORefC(Node* node);

Node* primUpdateVectorC(Node* node);

Node* primVectorIndexC(Node* node);

Node* primNewVectorC(Node* node);

Node* primCopyVectorC(Node* node);



/* autogenerated init function */
void init_YHC_Primitive(WrapRegisterFun reg, void* arg){
  reg("YHC;Primitive", "primAPIModuleLoad", Wrap_primAPIModuleLoad, arg);
  reg("YHC;Primitive", "primAPIModuleLookupNode", Wrap_primAPIModuleLookupNode, arg);
  reg("YHC;Primitive", "primAPIModuleLookupInfo", Wrap_primAPIModuleLookupInfo, arg);
  reg("YHC;Primitive", "primAPIModuleGetName", Wrap_primAPIModuleGetName, arg);
  reg("YHC;Primitive", "primAPIGetModule", Wrap_primAPIGetModule, arg);
  reg("YHC;Primitive", "primAPINewModule", Wrap_primAPINewModule, arg);
  reg("YHC;Primitive", "primAPIIsModuleLoaded", Wrap_primAPIIsModuleLoaded, arg);
  reg("YHC;Primitive", "primAPINewCInfo", Wrap_primAPINewCInfo, arg);
  reg("YHC;Primitive", "primAPIFInfoSetConstNode", Wrap_primAPIFInfoSetConstNode, arg);
  reg("YHC;Primitive", "primAPIFInfoSetConstInfo", Wrap_primAPIFInfoSetConstInfo, arg);
  reg("YHC;Primitive", "primAPINewFInfo", Wrap_primAPINewFInfo, arg);
  reg("YHC;Primitive", "primAPICInfoGetTag", Wrap_primAPICInfoGetTag, arg);
  reg("YHC;Primitive", "primAPICInfoGetName", Wrap_primAPICInfoGetName, arg);
  reg("YHC;Primitive", "primAPICInfoGetModule", Wrap_primAPICInfoGetModule, arg);
  reg("YHC;Primitive", "primAPICInfoGetSize", Wrap_primAPICInfoGetSize, arg);
  reg("YHC;Primitive", "primAPIFInfoGetConstNode", Wrap_primAPIFInfoGetConstNode, arg);
  reg("YHC;Primitive", "primAPIFInfoGetConstInfo", Wrap_primAPIFInfoGetConstInfo, arg);
  reg("YHC;Primitive", "primAPIFInfoGetConstType", Wrap_primAPIFInfoGetConstType, arg);
  reg("YHC;Primitive", "primAPIFInfoGetNumConsts", Wrap_primAPIFInfoGetNumConsts, arg);
  reg("YHC;Primitive", "primAPIFInfoGetCode", Wrap_primAPIFInfoGetCode, arg);
  reg("YHC;Primitive", "primAPIFInfoGetCodeSize", Wrap_primAPIFInfoGetCodeSize, arg);
  reg("YHC;Primitive", "primAPIFInfoGetName", Wrap_primAPIFInfoGetName, arg);
  reg("YHC;Primitive", "primAPIFInfoGetModule", Wrap_primAPIFInfoGetModule, arg);
  reg("YHC;Primitive", "primAPIFInfoGetStack", Wrap_primAPIFInfoGetStack, arg);
  reg("YHC;Primitive", "primAPIFInfoGetArity", Wrap_primAPIFInfoGetArity, arg);
  reg("YHC;Primitive", "primAPIFInfoGetPInfo", Wrap_primAPIFInfoGetPInfo, arg);
  reg("YHC;Primitive", "primAPIPInfoGetFInfo", Wrap_primAPIPInfoGetFInfo, arg);
  reg("YHC;Primitive", "primAPIPInfoGetNeed", Wrap_primAPIPInfoGetNeed, arg);
  reg("YHC;Primitive", "primAPIPInfoGetSize", Wrap_primAPIPInfoGetSize, arg);
  reg("YHC;Primitive", "primAPIInfoGetType", Wrap_primAPIInfoGetType, arg);
  reg("YHC;Primitive", "primAPINewNode", Wrap_primAPINewNode, arg);
  reg("YHC;Primitive", "primAPINodeIsNull", Wrap_primAPINodeIsNull, arg);
  reg("YHC;Primitive", "primAPINodeSetArg", Wrap_primAPINodeSetArg, arg);
  reg("YHC;Primitive", "primAPINodeGetArg", Wrap_primAPINodeGetArg, arg);
  reg("YHC;Primitive", "primAPINodeGetInfo", Wrap_primAPINodeGetInfo, arg);
  reg("YHC;Primitive", "primAPIFromNode", Wrap_primAPIFromNode, arg);
  reg("YHC;Primitive", "primAPIToNode", Wrap_primAPIToNode, arg);
  reg("YHC;Primitive", "primAPIUnlock", Wrap_primAPIUnlock, arg);
  reg("YHC;Primitive", "primAPILock", Wrap_primAPILock, arg);
  reg("YHC;Primitive", "primWriteInt64AtAddr", Wrap_primWriteInt64AtAddr, arg);
  reg("YHC;Primitive", "primReadInt64AtAddr", Wrap_primReadInt64AtAddr, arg);
  reg("YHC;Primitive", "primWriteInt32AtAddr", Wrap_primWriteInt32AtAddr, arg);
  reg("YHC;Primitive", "primReadInt32AtAddr", Wrap_primReadInt32AtAddr, arg);
  reg("YHC;Primitive", "primWriteInt16AtAddr", Wrap_primWriteInt16AtAddr, arg);
  reg("YHC;Primitive", "primReadInt16AtAddr", Wrap_primReadInt16AtAddr, arg);
  reg("YHC;Primitive", "primWriteInt8AtAddr", Wrap_primWriteInt8AtAddr, arg);
  reg("YHC;Primitive", "primReadInt8AtAddr", Wrap_primReadInt8AtAddr, arg);
  reg("YHC;Primitive", "primWriteWord64AtAddr", Wrap_primWriteWord64AtAddr, arg);
  reg("YHC;Primitive", "primReadWord64AtAddr", Wrap_primReadWord64AtAddr, arg);
  reg("YHC;Primitive", "primWriteWord32AtAddr", Wrap_primWriteWord32AtAddr, arg);
  reg("YHC;Primitive", "primReadWord32AtAddr", Wrap_primReadWord32AtAddr, arg);
  reg("YHC;Primitive", "primWriteWord16AtAddr", Wrap_primWriteWord16AtAddr, arg);
  reg("YHC;Primitive", "primReadWord16AtAddr", Wrap_primReadWord16AtAddr, arg);
  reg("YHC;Primitive", "primWriteWord8AtAddr", Wrap_primWriteWord8AtAddr, arg);
  reg("YHC;Primitive", "primReadWord8AtAddr", Wrap_primReadWord8AtAddr, arg);
  reg("YHC;Primitive", "primWriteAddrAtAddr", Wrap_primWriteAddrAtAddr, arg);
  reg("YHC;Primitive", "primReadAddrAtAddr", Wrap_primReadAddrAtAddr, arg);
  reg("YHC;Primitive", "primWriteDoubleAtAddr", Wrap_primWriteDoubleAtAddr, arg);
  reg("YHC;Primitive", "primReadDoubleAtAddr", Wrap_primReadDoubleAtAddr, arg);
  reg("YHC;Primitive", "primWriteFloatAtAddr", Wrap_primWriteFloatAtAddr, arg);
  reg("YHC;Primitive", "primReadFloatAtAddr", Wrap_primReadFloatAtAddr, arg);
  reg("YHC;Primitive", "primWriteIntAtAddr", Wrap_primWriteIntAtAddr, arg);
  reg("YHC;Primitive", "primReadIntAtAddr", Wrap_primReadIntAtAddr, arg);
  reg("YHC;Primitive", "primWriteCharAtAddr", Wrap_primWriteCharAtAddr, arg);
  reg("YHC;Primitive", "primReadCharAtAddr", Wrap_primReadCharAtAddr, arg);
  reg("YHC;Primitive", "primWord64ToInteger", Wrap_primWord64ToInteger, arg);
  reg("YHC;Primitive", "primWord64FromInteger", Wrap_primWord64FromInteger, arg);
  reg("YHC;Primitive", "primFromEnumWord64", Wrap_primFromEnumWord64, arg);
  reg("YHC;Primitive", "primToEnumWord64", Wrap_primToEnumWord64, arg);
  reg("YHC;Primitive", "primRemWord64", Wrap_primRemWord64, arg);
  reg("YHC;Primitive", "primQuotWord64", Wrap_primQuotWord64, arg);
  reg("YHC;Primitive", "primSignumWord64", Wrap_primSignumWord64, arg);
  reg("YHC;Primitive", "primAbsWord64", Wrap_primAbsWord64, arg);
  reg("YHC;Primitive", "primMulWord64", Wrap_primMulWord64, arg);
  reg("YHC;Primitive", "primSubWord64", Wrap_primSubWord64, arg);
  reg("YHC;Primitive", "primAddWord64", Wrap_primAddWord64, arg);
  reg("YHC;Primitive", "primGeWord64", Wrap_primGeWord64, arg);
  reg("YHC;Primitive", "primGtWord64", Wrap_primGtWord64, arg);
  reg("YHC;Primitive", "primLeWord64", Wrap_primLeWord64, arg);
  reg("YHC;Primitive", "primLtWord64", Wrap_primLtWord64, arg);
  reg("YHC;Primitive", "primEqWord64", Wrap_primEqWord64, arg);
  reg("YHC;Primitive", "primWord32ToInteger", Wrap_primWord32ToInteger, arg);
  reg("YHC;Primitive", "primWord32FromInteger", Wrap_primWord32FromInteger, arg);
  reg("YHC;Primitive", "primFromEnumWord32", Wrap_primFromEnumWord32, arg);
  reg("YHC;Primitive", "primToEnumWord32", Wrap_primToEnumWord32, arg);
  reg("YHC;Primitive", "primRemWord32", Wrap_primRemWord32, arg);
  reg("YHC;Primitive", "primQuotWord32", Wrap_primQuotWord32, arg);
  reg("YHC;Primitive", "primSignumWord32", Wrap_primSignumWord32, arg);
  reg("YHC;Primitive", "primAbsWord32", Wrap_primAbsWord32, arg);
  reg("YHC;Primitive", "primMulWord32", Wrap_primMulWord32, arg);
  reg("YHC;Primitive", "primSubWord32", Wrap_primSubWord32, arg);
  reg("YHC;Primitive", "primAddWord32", Wrap_primAddWord32, arg);
  reg("YHC;Primitive", "primGeWord32", Wrap_primGeWord32, arg);
  reg("YHC;Primitive", "primGtWord32", Wrap_primGtWord32, arg);
  reg("YHC;Primitive", "primLeWord32", Wrap_primLeWord32, arg);
  reg("YHC;Primitive", "primLtWord32", Wrap_primLtWord32, arg);
  reg("YHC;Primitive", "primEqWord32", Wrap_primEqWord32, arg);
  reg("YHC;Primitive", "primWord16ToInteger", Wrap_primWord16ToInteger, arg);
  reg("YHC;Primitive", "primWord16FromInteger", Wrap_primWord16FromInteger, arg);
  reg("YHC;Primitive", "primFromEnumWord16", Wrap_primFromEnumWord16, arg);
  reg("YHC;Primitive", "primToEnumWord16", Wrap_primToEnumWord16, arg);
  reg("YHC;Primitive", "primRemWord16", Wrap_primRemWord16, arg);
  reg("YHC;Primitive", "primQuotWord16", Wrap_primQuotWord16, arg);
  reg("YHC;Primitive", "primSignumWord16", Wrap_primSignumWord16, arg);
  reg("YHC;Primitive", "primAbsWord16", Wrap_primAbsWord16, arg);
  reg("YHC;Primitive", "primMulWord16", Wrap_primMulWord16, arg);
  reg("YHC;Primitive", "primSubWord16", Wrap_primSubWord16, arg);
  reg("YHC;Primitive", "primAddWord16", Wrap_primAddWord16, arg);
  reg("YHC;Primitive", "primGeWord16", Wrap_primGeWord16, arg);
  reg("YHC;Primitive", "primGtWord16", Wrap_primGtWord16, arg);
  reg("YHC;Primitive", "primLeWord16", Wrap_primLeWord16, arg);
  reg("YHC;Primitive", "primLtWord16", Wrap_primLtWord16, arg);
  reg("YHC;Primitive", "primEqWord16", Wrap_primEqWord16, arg);
  reg("YHC;Primitive", "primWord8ToInteger", Wrap_primWord8ToInteger, arg);
  reg("YHC;Primitive", "primWord8FromInteger", Wrap_primWord8FromInteger, arg);
  reg("YHC;Primitive", "primFromEnumWord8", Wrap_primFromEnumWord8, arg);
  reg("YHC;Primitive", "primToEnumWord8", Wrap_primToEnumWord8, arg);
  reg("YHC;Primitive", "primRemWord8", Wrap_primRemWord8, arg);
  reg("YHC;Primitive", "primQuotWord8", Wrap_primQuotWord8, arg);
  reg("YHC;Primitive", "primSignumWord8", Wrap_primSignumWord8, arg);
  reg("YHC;Primitive", "primAbsWord8", Wrap_primAbsWord8, arg);
  reg("YHC;Primitive", "primMulWord8", Wrap_primMulWord8, arg);
  reg("YHC;Primitive", "primSubWord8", Wrap_primSubWord8, arg);
  reg("YHC;Primitive", "primAddWord8", Wrap_primAddWord8, arg);
  reg("YHC;Primitive", "primGeWord8", Wrap_primGeWord8, arg);
  reg("YHC;Primitive", "primGtWord8", Wrap_primGtWord8, arg);
  reg("YHC;Primitive", "primLeWord8", Wrap_primLeWord8, arg);
  reg("YHC;Primitive", "primLtWord8", Wrap_primLtWord8, arg);
  reg("YHC;Primitive", "primEqWord8", Wrap_primEqWord8, arg);
  reg("YHC;Primitive", "primInt64ToInteger", Wrap_primInt64ToInteger, arg);
  reg("YHC;Primitive", "primInt64FromInteger", Wrap_primInt64FromInteger, arg);
  reg("YHC;Primitive", "primFromEnumInt64", Wrap_primFromEnumInt64, arg);
  reg("YHC;Primitive", "primToEnumInt64", Wrap_primToEnumInt64, arg);
  reg("YHC;Primitive", "primRemInt64", Wrap_primRemInt64, arg);
  reg("YHC;Primitive", "primQuotInt64", Wrap_primQuotInt64, arg);
  reg("YHC;Primitive", "primSignumInt64", Wrap_primSignumInt64, arg);
  reg("YHC;Primitive", "primAbsInt64", Wrap_primAbsInt64, arg);
  reg("YHC;Primitive", "primMulInt64", Wrap_primMulInt64, arg);
  reg("YHC;Primitive", "primSubInt64", Wrap_primSubInt64, arg);
  reg("YHC;Primitive", "primAddInt64", Wrap_primAddInt64, arg);
  reg("YHC;Primitive", "primGeInt64", Wrap_primGeInt64, arg);
  reg("YHC;Primitive", "primGtInt64", Wrap_primGtInt64, arg);
  reg("YHC;Primitive", "primLeInt64", Wrap_primLeInt64, arg);
  reg("YHC;Primitive", "primLtInt64", Wrap_primLtInt64, arg);
  reg("YHC;Primitive", "primEqInt64", Wrap_primEqInt64, arg);
  reg("YHC;Primitive", "primInt32ToInteger", Wrap_primInt32ToInteger, arg);
  reg("YHC;Primitive", "primInt32FromInteger", Wrap_primInt32FromInteger, arg);
  reg("YHC;Primitive", "primFromEnumInt32", Wrap_primFromEnumInt32, arg);
  reg("YHC;Primitive", "primToEnumInt32", Wrap_primToEnumInt32, arg);
  reg("YHC;Primitive", "primRemInt32", Wrap_primRemInt32, arg);
  reg("YHC;Primitive", "primQuotInt32", Wrap_primQuotInt32, arg);
  reg("YHC;Primitive", "primSignumInt32", Wrap_primSignumInt32, arg);
  reg("YHC;Primitive", "primAbsInt32", Wrap_primAbsInt32, arg);
  reg("YHC;Primitive", "primMulInt32", Wrap_primMulInt32, arg);
  reg("YHC;Primitive", "primSubInt32", Wrap_primSubInt32, arg);
  reg("YHC;Primitive", "primAddInt32", Wrap_primAddInt32, arg);
  reg("YHC;Primitive", "primGeInt32", Wrap_primGeInt32, arg);
  reg("YHC;Primitive", "primGtInt32", Wrap_primGtInt32, arg);
  reg("YHC;Primitive", "primLeInt32", Wrap_primLeInt32, arg);
  reg("YHC;Primitive", "primLtInt32", Wrap_primLtInt32, arg);
  reg("YHC;Primitive", "primEqInt32", Wrap_primEqInt32, arg);
  reg("YHC;Primitive", "primInt16ToInteger", Wrap_primInt16ToInteger, arg);
  reg("YHC;Primitive", "primInt16FromInteger", Wrap_primInt16FromInteger, arg);
  reg("YHC;Primitive", "primFromEnumInt16", Wrap_primFromEnumInt16, arg);
  reg("YHC;Primitive", "primToEnumInt16", Wrap_primToEnumInt16, arg);
  reg("YHC;Primitive", "primRemInt16", Wrap_primRemInt16, arg);
  reg("YHC;Primitive", "primQuotInt16", Wrap_primQuotInt16, arg);
  reg("YHC;Primitive", "primSignumInt16", Wrap_primSignumInt16, arg);
  reg("YHC;Primitive", "primAbsInt16", Wrap_primAbsInt16, arg);
  reg("YHC;Primitive", "primMulInt16", Wrap_primMulInt16, arg);
  reg("YHC;Primitive", "primSubInt16", Wrap_primSubInt16, arg);
  reg("YHC;Primitive", "primAddInt16", Wrap_primAddInt16, arg);
  reg("YHC;Primitive", "primGeInt16", Wrap_primGeInt16, arg);
  reg("YHC;Primitive", "primGtInt16", Wrap_primGtInt16, arg);
  reg("YHC;Primitive", "primLeInt16", Wrap_primLeInt16, arg);
  reg("YHC;Primitive", "primLtInt16", Wrap_primLtInt16, arg);
  reg("YHC;Primitive", "primEqInt16", Wrap_primEqInt16, arg);
  reg("YHC;Primitive", "primInt8ToInteger", Wrap_primInt8ToInteger, arg);
  reg("YHC;Primitive", "primInt8FromInteger", Wrap_primInt8FromInteger, arg);
  reg("YHC;Primitive", "primFromEnumInt8", Wrap_primFromEnumInt8, arg);
  reg("YHC;Primitive", "primToEnumInt8", Wrap_primToEnumInt8, arg);
  reg("YHC;Primitive", "primRemInt8", Wrap_primRemInt8, arg);
  reg("YHC;Primitive", "primQuotInt8", Wrap_primQuotInt8, arg);
  reg("YHC;Primitive", "primSignumInt8", Wrap_primSignumInt8, arg);
  reg("YHC;Primitive", "primAbsInt8", Wrap_primAbsInt8, arg);
  reg("YHC;Primitive", "primMulInt8", Wrap_primMulInt8, arg);
  reg("YHC;Primitive", "primSubInt8", Wrap_primSubInt8, arg);
  reg("YHC;Primitive", "primAddInt8", Wrap_primAddInt8, arg);
  reg("YHC;Primitive", "primGeInt8", Wrap_primGeInt8, arg);
  reg("YHC;Primitive", "primGtInt8", Wrap_primGtInt8, arg);
  reg("YHC;Primitive", "primLeInt8", Wrap_primLeInt8, arg);
  reg("YHC;Primitive", "primLtInt8", Wrap_primLtInt8, arg);
  reg("YHC;Primitive", "primEqInt8", Wrap_primEqInt8, arg);
  reg("YHC;Primitive", "primMemmove", Wrap_primMemmove, arg);
  reg("YHC;Primitive", "primMemcpy", Wrap_primMemcpy, arg);
  reg("YHC;Primitive", "primFinalizerFree", Wrap_primFinalizerFree, arg);
  reg("YHC;Primitive", "primRealloc", Wrap_primRealloc, arg);
  reg("YHC;Primitive", "primFree", Wrap_primFree, arg);
  reg("YHC;Primitive", "primMalloc", Wrap_primMalloc, arg);
  reg("YHC;Primitive", "primFreeCString", Wrap_primFreeCString, arg);
  reg("YHC;Primitive", "primNewCString", primNewCString, arg);
  reg("YHC;Primitive", "getErrorNo", Wrap_getErrorNo, arg);
  reg("YHC;Primitive", "primDerefStablePtr", Wrap_primDerefStablePtr, arg);
  reg("YHC;Primitive", "primFreeStablePtr", Wrap_primFreeStablePtr, arg);
  reg("YHC;Primitive", "primCreateStablePtr", Wrap_primCreateStablePtr, arg);
  reg("YHC;Primitive", "primFreeForeignPtr", primFreeForeignPtr, arg);
  reg("YHC;Primitive", "primDerefForeignPtr", primDerefForeignPtr, arg);
  reg("YHC;Primitive", "primAttachForeignPtr", primAttachForeignPtr, arg);
  reg("YHC;Primitive", "primCreateForeignPtr", primCreateForeignPtr, arg);
  reg("YHC;Primitive", "castPtrToFunPtr", Wrap_castPtrToFunPtr, arg);
  reg("YHC;Primitive", "castFunPtrToPtr", Wrap_castFunPtrToPtr, arg);
  reg("YHC;Primitive", "funPtrToInt", Wrap_funPtrToInt, arg);
  reg("YHC;Primitive", "castPtr", Wrap_castPtr, arg);
  reg("YHC;Primitive", "intToPtr", Wrap_intToPtr, arg);
  reg("YHC;Primitive", "ptrToInt", Wrap_ptrToInt, arg);
  reg("YHC;Primitive", "primTryTakeMVar", primTryTakeMVar, arg);
  reg("YHC;Primitive", "primIsEmptyMVar", primIsEmptyMVar, arg);
  reg("YHC;Primitive", "primSwapMVar", primSwapMVar, arg);
  reg("YHC;Primitive", "primReadMVar", primReadMVar, arg);
  reg("YHC;Primitive", "primTakeMVar", primTakeMVar, arg);
  reg("YHC;Primitive", "primPutMVar", primPutMVar, arg);
  reg("YHC;Primitive", "primNewMVar", primNewMVar, arg);
  reg("YHC;Primitive", "primMyThreadId", primMyThreadId, arg);
  reg("YHC;Primitive", "primSpinLockProcess", primSpinLockProcess, arg);
  reg("YHC;Primitive", "primKillProcess", primKillProcess, arg);
  reg("YHC;Primitive", "primSpawnProcess", primSpawnProcess, arg);
  reg("YHC;Primitive", "primUnblockExceptions", Wrap_primUnblockExceptions, arg);
  reg("YHC;Primitive", "primBlockExceptions", Wrap_primBlockExceptions, arg);
  reg("YHC;Primitive", "primDirExists", Wrap_primDirExists, arg);
  reg("YHC;Primitive", "primFileExists", Wrap_primFileExists, arg);
  reg("YHC;Primitive", "primCopyFile", Wrap_primCopyFile, arg);
  reg("YHC;Primitive", "primRenameFile", Wrap_primRenameFile, arg);
  reg("YHC;Primitive", "primRemoveFile", Wrap_primRemoveFile, arg);
  reg("YHC;Primitive", "primGetHomeDir", Wrap_primGetHomeDir, arg);
  reg("YHC;Primitive", "primSetCurrentDir", Wrap_primSetCurrentDir, arg);
  reg("YHC;Primitive", "primGetCurrentDir", Wrap_primGetCurrentDir, arg);
  reg("YHC;Primitive", "primCloseDir", Wrap_primCloseDir, arg);
  reg("YHC;Primitive", "primNextDir", Wrap_primNextDir, arg);
  reg("YHC;Primitive", "primGetDirName", Wrap_primGetDirName, arg);
  reg("YHC;Primitive", "primOpenDir", Wrap_primOpenDir, arg);
  reg("YHC;Primitive", "primRenameDir", Wrap_primRenameDir, arg);
  reg("YHC;Primitive", "primRemoveDir", Wrap_primRemoveDir, arg);
  reg("YHC;Primitive", "primCreateDir", Wrap_primCreateDir, arg);
  reg("YHC;Primitive", "primGetEnv", Wrap_primGetEnv, arg);
  reg("YHC;Primitive", "primGetArg", Wrap_primGetArg, arg);
  reg("YHC;Primitive", "primGetProgName", Wrap_primGetProgName, arg);
  reg("YHC;Primitive", "primExitWith", Wrap_primExitWith, arg);
  reg("YHC;Primitive", "stdinC", Wrap_stdinC, arg);
  reg("YHC;Primitive", "stderrC", Wrap_stderrC, arg);
  reg("YHC;Primitive", "stdoutC", Wrap_stdoutC, arg);
  reg("YHC;Primitive", "openFileC", Wrap_openFileC, arg);
  reg("YHC;Primitive", "hTellC", Wrap_hTellC, arg);
  reg("YHC;Primitive", "hGetBufferingC", Wrap_hGetBufferingC, arg);
  reg("YHC;Primitive", "hSetBufferingC", Wrap_hSetBufferingC, arg);
  reg("YHC;Primitive", "hSeekC", Wrap_hSeekC, arg);
  reg("YHC;Primitive", "hPutCharC", Wrap_hPutCharC, arg);
  reg("YHC;Primitive", "hIsEOFC", Wrap_hIsEOFC, arg);
  reg("YHC;Primitive", "hGetTypeC", Wrap_hGetTypeC, arg);
  reg("YHC;Primitive", "hGetFileNameC", Wrap_hGetFileNameC, arg);
  reg("YHC;Primitive", "hFlushC", Wrap_hFlushC, arg);
  reg("YHC;Primitive", "hGetCharC", Wrap_hGetCharC, arg);
  reg("YHC;Primitive", "hFileSizeC", Wrap_hFileSizeC, arg);
  reg("YHC;Primitive", "hCloseC", Wrap_hCloseC, arg);
  reg("YHC;Primitive", "primEqHandleC", Wrap_primEqHandleC, arg);
  reg("YHC;Primitive", "hGetErrorC", Wrap_hGetErrorC, arg);
  reg("YHC;Primitive", "primUnsafeCoerce", primUnsafeCoerce, arg);
  reg("YHC;Primitive", "primStrError", primStrError, arg);
  reg("YHC;Primitive", "primIntegerRem", primIntegerRem, arg);
  reg("YHC;Primitive", "primIntegerQuot", primIntegerQuot, arg);
  reg("YHC;Primitive", "primIntegerQuotRem", primIntegerQuotRem, arg);
  reg("YHC;Primitive", "primIntegerNeg", primIntegerNeg, arg);
  reg("YHC;Primitive", "primIntegerMul", primIntegerMul, arg);
  reg("YHC;Primitive", "primIntegerSub", primIntegerSub, arg);
  reg("YHC;Primitive", "primIntegerAdd", primIntegerAdd, arg);
  reg("YHC;Primitive", "primIntegerGt", primIntegerGt, arg);
  reg("YHC;Primitive", "primIntegerGe", primIntegerGe, arg);
  reg("YHC;Primitive", "primIntegerLe", primIntegerLe, arg);
  reg("YHC;Primitive", "primIntegerLt", primIntegerLt, arg);
  reg("YHC;Primitive", "primIntegerNe", primIntegerNe, arg);
  reg("YHC;Primitive", "primIntegerEq", primIntegerEq, arg);
  reg("YHC;Primitive", "primEncodeDouble", primEncodeDouble, arg);
  reg("YHC;Primitive", "primDecodeDouble", primDecodeDouble, arg);
  reg("YHC;Primitive", "primDoubleSignum", primDoubleSignum, arg);
  reg("YHC;Primitive", "primDoubleAbs", primDoubleAbs, arg);
  reg("YHC;Primitive", "primDoubleFromInteger", primDoubleFromInteger, arg);
  reg("YHC;Primitive", "primDoublePow", primDoublePow, arg);
  reg("YHC;Primitive", "primDoubleATan", primDoubleATan, arg);
  reg("YHC;Primitive", "primDoubleACos", primDoubleACos, arg);
  reg("YHC;Primitive", "primDoubleASin", primDoubleASin, arg);
  reg("YHC;Primitive", "primDoubleTan", primDoubleTan, arg);
  reg("YHC;Primitive", "primDoubleCos", primDoubleCos, arg);
  reg("YHC;Primitive", "primDoubleSin", primDoubleSin, arg);
  reg("YHC;Primitive", "primDoubleSqrt", primDoubleSqrt, arg);
  reg("YHC;Primitive", "primDoubleLog", primDoubleLog, arg);
  reg("YHC;Primitive", "primDoubleExp", primDoubleExp, arg);
  reg("YHC;Primitive", "primEncodeFloat", primEncodeFloat, arg);
  reg("YHC;Primitive", "primDecodeFloat", primDecodeFloat, arg);
  reg("YHC;Primitive", "primFloatSignum", primFloatSignum, arg);
  reg("YHC;Primitive", "primFloatAbs", primFloatAbs, arg);
  reg("YHC;Primitive", "primFloatFromInteger", primFloatFromInteger, arg);
  reg("YHC;Primitive", "primFloatPow", primFloatPow, arg);
  reg("YHC;Primitive", "primFloatATan", primFloatATan, arg);
  reg("YHC;Primitive", "primFloatACos", primFloatACos, arg);
  reg("YHC;Primitive", "primFloatASin", primFloatASin, arg);
  reg("YHC;Primitive", "primFloatTan", primFloatTan, arg);
  reg("YHC;Primitive", "primFloatCos", primFloatCos, arg);
  reg("YHC;Primitive", "primFloatSin", primFloatSin, arg);
  reg("YHC;Primitive", "primFloatSqrt", primFloatSqrt, arg);
  reg("YHC;Primitive", "primFloatLog", primFloatLog, arg);
  reg("YHC;Primitive", "primFloatExp", primFloatExp, arg);
  reg("YHC;Primitive", "primIntegerFromInt", primIntegerFromInt, arg);
  reg("YHC;Primitive", "primIntFromInteger", primIntFromInteger, arg);
  reg("YHC;Primitive", "primIntSignum", primIntSignum, arg);
  reg("YHC;Primitive", "primIntAbs", primIntAbs, arg);
  reg("YHC;Primitive", "primComparePS", Wrap_primComparePS, arg);
  reg("YHC;Primitive", "primPSGetChar", Wrap_primPSGetChar, arg);
  reg("YHC;Primitive", "primPackString", primPackString, arg);
  reg("YHC;Primitive", "primWriteIORefC", primWriteIORefC, arg);
  reg("YHC;Primitive", "primReadIORefC", primReadIORefC, arg);
  reg("YHC;Primitive", "primNewIORefC", primNewIORefC, arg);
  reg("YHC;Primitive", "primUpdateVectorC", primUpdateVectorC, arg);
  reg("YHC;Primitive", "primVectorIndexC", primVectorIndexC, arg);
  reg("YHC;Primitive", "primNewVectorC", primNewVectorC, arg);
  reg("YHC;Primitive", "primCopyVectorC", primCopyVectorC, arg);
}
