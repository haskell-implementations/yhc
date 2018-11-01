#include "../hsffi.h"
#include "../make.h"
#include "../integer.h"
#include "../stable.h"
#include <errno.h>

/*---------------------------------------------------------------------------------------------------
   C.Error
  ---------------------------------------------------------------------------------------------------*/

Int getErrorNo(){
  return errno;
}

/*---------------------------------------------------------------------------------------------------
   C.String
  ---------------------------------------------------------------------------------------------------*/

Node* primNewCString(Node* node){
  Node* len = (Node*)node->args[0];
  Node* cons = node->args[1];
  Global gCons = { NULL, &cons };
  StringNode* res;
  Char* p;
  Int i, iLen;

  REMOVE_IND(len, Node*);
  iLen = ((INode*)len)->value;

  heap_pushGlobal(&gCons);
  res = (StringNode*)heap_alloc(wordsof(StringNode));
  heap_popGlobal();
  res->string = (Char*)malloc(sizeof(Char) * (iLen+1));

  MAKE_NODE(res, &G_infoString, N_NORMAL);

  for (i = 0, p = res->string; i < iLen; i++, p++){
    INode* cnode;

    REMOVE_IND(cons, Node*);
    cnode = (INode*)cons->args[0];
    REMOVE_IND(cnode, INode*);
    *p = (Char)cnode->value;
    cons = cons->args[1];
  }
  *p = '\0';
  return (Node*)res;
}

void primFreeCString(Char* str){
  free(str);
}

/*---------------------------------------------------------------------------------------------------
   Marshal.Alloc
  ---------------------------------------------------------------------------------------------------*/

void* primMalloc(Int bytes){
  return malloc(bytes);
}

void* primRealloc(void* ptr, Int bytes){
  return realloc(ptr, bytes);
}

void* primFinalizerFree(){
  return free;
}

void primFree(void* mem){
  free(mem);
}

/*---------------------------------------------------------------------------------------------------
   Marshal.Utils
  ---------------------------------------------------------------------------------------------------*/

void primMemcpy(void* dst, void* src, Int nbytes){
  memcpy(dst, src, nbytes);
}

void primMemmove(void* dst, void* src, Int nbytes){
  memmove(dst, src, nbytes);
}

/*---------------------------------------------------------------------------------------------------
   ForeignPtr
  ---------------------------------------------------------------------------------------------------*/

Node* primCreateForeignPtr(Node* node){
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  return foreign_create(UNBOX_PTR(arg0));
}

Node* primAttachForeignPtr(Node* node){
  Node* arg0 = node->args[0];
  Node* arg1 = node->args[1];
  REMOVE_IND(arg0, Node*);
  REMOVE_IND(arg0, Node*);
  foreign_attach((FinalizerCallback)UNBOX_FUN_PTR(arg0),
                 (ForeignPtr*)UNBOX_PTR(arg1));
  return NODE_UNIT;
}

Node* primDerefForeignPtr(Node* node){
  void* pResult;
  Node* nResult;
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  pResult = FOREIGN_DEREF((ForeignPtr*)UNBOX_PTR(arg0));
  BOX_PTR(nResult, pResult);
  return nResult;
}

Node* primFreeForeignPtr(Node* node){
  Node* arg0 = node->args[0];
  REMOVE_IND(arg0, Node*);
  foreign_free((ForeignPtr*)UNBOX_PTR(arg0));
  return NODE_UNIT;
}

/*---------------------------------------------------------------------------------------------------
   StablePtr
  ---------------------------------------------------------------------------------------------------*/

StablePtr* primCreateStablePtr(Node* node){
  return stable_create(node);
}

void primFreeStablePtr(StablePtr* p){
  stable_free(p);
}

Node* primDerefStablePtr(StablePtr* p){
  return p->node;
}

/*---------------------------------------------------------------------------------------------------
   Int
  ---------------------------------------------------------------------------------------------------*/

#define I_CMP_OP(T,N,op) \
  Bool prim##N##T(T x, T y){ return x op y; }

#define I_BIN_OP(T,N,op) \
  T prim##N##T(T x, T y){ return x op y; }

#define I_FUNS(T) \
  I_CMP_OP(T,Eq,==) \
  I_CMP_OP(T,Ne,!=) \
  I_CMP_OP(T,Lt,<) \
  I_CMP_OP(T,Le,<=) \
  I_CMP_OP(T,Gt,>) \
  I_CMP_OP(T,Ge,>=) \
  I_BIN_OP(T,Add,+) \
  I_BIN_OP(T,Sub,-) \
  I_BIN_OP(T,Mul,*) \
  I_BIN_OP(T,Quot,/) \
  I_BIN_OP(T,Rem,%) \
  T primAbs##T(T x){ return (x < 0) ? -x : x; } \
  T primSignum##T(T x){ return (x < 0) ? -1 : (x > 0) ? 1 : 0; } \
  T primToEnum##T(Int x){ return (T)x; } \
  Int primFromEnum##T(T x){ return (Int)x; } \
  Node* prim##T##ToInteger(T x){ return (Node*)make_integer_64(x < 0, (UInt64)ABS(x)); } \
  T prim##T##FromInteger(Node* x){ return (T)integer_value_u64((IntegerNode*)x); }

I_FUNS(Int8)
I_FUNS(Int16)
I_FUNS(Int32)
I_FUNS(Int64)

/*---------------------------------------------------------------------------------------------------
   Word
  ---------------------------------------------------------------------------------------------------*/

typedef UInt8 Word8;
typedef UInt16 Word16;
typedef UInt32 Word32;
typedef UInt64 Word64;

#define W_CMP_OP(T,N,op) \
  Bool prim##N##T(T x, T y){ return x op y; }

#define W_BIN_OP(T,N,op) \
  T prim##N##T(T x, T y){ return x op y; }

#define W_FUNS(T) \
  W_CMP_OP(T,Eq,==) \
  W_CMP_OP(T,Ne,!=) \
  W_CMP_OP(T,Lt,<) \
  W_CMP_OP(T,Le,<=) \
  W_CMP_OP(T,Gt,>) \
  W_CMP_OP(T,Ge,>=) \
  W_BIN_OP(T,Add,+) \
  W_BIN_OP(T,Sub,-) \
  W_BIN_OP(T,Mul,*) \
  W_BIN_OP(T,Quot,/) \
  W_BIN_OP(T,Rem,%) \
  T primAbs##T(T x){ return x; } \
  T primSignum##T(T x){ return (x > 0) ? 1 : 0; } \
  T primToEnum##T(Int x){ return (T)x; } \
  Int primFromEnum##T(T x){ return (Int)x; } \
  Node* prim##T##ToInteger(T x){ return (Node*)make_integer_64(0, (UInt64)x); } \
  T prim##T##FromInteger(Node* x){ return (T)integer_value_u64((IntegerNode*)x); }

W_FUNS(Word8)
W_FUNS(Word16)
W_FUNS(Word32)
W_FUNS(Word64)

/*---------------------------------------------------------------------------------------------------
   Storable
  ---------------------------------------------------------------------------------------------------*/

typedef void* Addr;

#define S_FUNS(T) \
  T primRead##T##AtAddr(T* p){ return *p; } \
  void primWrite##T##AtAddr(T* p, T i){ *p = i; }

S_FUNS(Char)
S_FUNS(Int)
S_FUNS(Float)
S_FUNS(Double)
S_FUNS(Addr)
S_FUNS(Word8)
S_FUNS(Word16)
S_FUNS(Word32)
S_FUNS(Word64)
S_FUNS(Int8)
S_FUNS(Int16)
S_FUNS(Int32)
S_FUNS(Int64)

void primFFI_init(){
  prim_addFun("getErrorNo", getErrorNo);

  prim_addFun("primNewCString", primNewCString);
  prim_addFun("primFreeCString", primFreeCString);

  prim_addFun("primCreateForeignPtr", primCreateForeignPtr);
  prim_addFun("primAttachForeignPtr", primAttachForeignPtr);
  prim_addFun("primDerefForeignPtr", primDerefForeignPtr);
  prim_addFun("primFreeForeignPtr", primFreeForeignPtr);

  prim_addFun("primCreateStablePtr", primCreateStablePtr);
  prim_addFun("primFreeStablePtr", primFreeStablePtr);
  prim_addFun("primDerefStablePtr", primDerefStablePtr);

  prim_addFun("primFromEnumInt64", primFromEnumInt64);
  prim_addFun("primToEnumInt64", primToEnumInt64);
  prim_addFun("primInt64ToInteger", primInt64ToInteger);
  prim_addFun("primRemInt64", primRemInt64);
  prim_addFun("primQuotInt64", primQuotInt64);
  prim_addFun("primInt64FromInteger", primInt64FromInteger);
  prim_addFun("primSignumInt64", primSignumInt64);
  prim_addFun("primAbsInt64", primAbsInt64);
  prim_addFun("primMulInt64", primMulInt64);
  prim_addFun("primSubInt64", primSubInt64);
  prim_addFun("primAddInt64", primAddInt64);
  prim_addFun("primGeInt64", primGeInt64);
  prim_addFun("primGtInt64", primGtInt64);
  prim_addFun("primLeInt64", primLeInt64);
  prim_addFun("primLtInt64", primLtInt64);
  prim_addFun("primEqInt64", primEqInt64);
  prim_addFun("primFromEnumInt32", primFromEnumInt32);
  prim_addFun("primToEnumInt32", primToEnumInt32);
  prim_addFun("primInt32ToInteger", primInt32ToInteger);
  prim_addFun("primRemInt32", primRemInt32);
  prim_addFun("primQuotInt32", primQuotInt32);
  prim_addFun("primInt32FromInteger", primInt32FromInteger);
  prim_addFun("primSignumInt32", primSignumInt32);
  prim_addFun("primAbsInt32", primAbsInt32);
  prim_addFun("primMulInt32", primMulInt32);
  prim_addFun("primSubInt32", primSubInt32);
  prim_addFun("primAddInt32", primAddInt32);
  prim_addFun("primGeInt32", primGeInt32);
  prim_addFun("primGtInt32", primGtInt32);
  prim_addFun("primLeInt32", primLeInt32);
  prim_addFun("primLtInt32", primLtInt32);
  prim_addFun("primEqInt32", primEqInt32);
  prim_addFun("primFromEnumInt16", primFromEnumInt16);
  prim_addFun("primToEnumInt16", primToEnumInt16);
  prim_addFun("primInt16ToInteger", primInt16ToInteger);
  prim_addFun("primRemInt16", primRemInt16);
  prim_addFun("primQuotInt16", primQuotInt16);
  prim_addFun("primInt16FromInteger", primInt16FromInteger);
  prim_addFun("primSignumInt16", primSignumInt16);
  prim_addFun("primAbsInt16", primAbsInt16);
  prim_addFun("primMulInt16", primMulInt16);
  prim_addFun("primSubInt16", primSubInt16);
  prim_addFun("primAddInt16", primAddInt16);
  prim_addFun("primGeInt16", primGeInt16);
  prim_addFun("primGtInt16", primGtInt16);
  prim_addFun("primLeInt16", primLeInt16);
  prim_addFun("primLtInt16", primLtInt16);
  prim_addFun("primEqInt16", primEqInt16);
  prim_addFun("primFromEnumInt8", primFromEnumInt8);
  prim_addFun("primToEnumInt8", primToEnumInt8);
  prim_addFun("primInt8ToInteger", primInt8ToInteger);
  prim_addFun("primRemInt8", primRemInt8);
  prim_addFun("primQuotInt8", primQuotInt8);
  prim_addFun("primInt8FromInteger", primInt8FromInteger);
  prim_addFun("primSignumInt8", primSignumInt8);
  prim_addFun("primAbsInt8", primAbsInt8);
  prim_addFun("primMulInt8", primMulInt8);
  prim_addFun("primSubInt8", primSubInt8);
  prim_addFun("primAddInt8", primAddInt8);
  prim_addFun("primGeInt8", primGeInt8);
  prim_addFun("primGtInt8", primGtInt8);
  prim_addFun("primLeInt8", primLeInt8);
  prim_addFun("primLtInt8", primLtInt8);
  prim_addFun("primEqInt8", primEqInt8);

  prim_addFun("primFromEnumWord64", primFromEnumWord64);
  prim_addFun("primToEnumWord64", primToEnumWord64);
  prim_addFun("primWord64ToInteger", primWord64ToInteger);
  prim_addFun("primRemWord64", primRemWord64);
  prim_addFun("primQuotWord64", primQuotWord64);
  prim_addFun("primWord64FromInteger", primWord64FromInteger);
  prim_addFun("primSignumWord64", primSignumWord64);
  prim_addFun("primAbsWord64", primAbsWord64);
  prim_addFun("primMulWord64", primMulWord64);
  prim_addFun("primSubWord64", primSubWord64);
  prim_addFun("primAddWord64", primAddWord64);
  prim_addFun("primGeWord64", primGeWord64);
  prim_addFun("primGtWord64", primGtWord64);
  prim_addFun("primLeWord64", primLeWord64);
  prim_addFun("primLtWord64", primLtWord64);
  prim_addFun("primEqWord64", primEqWord64);
  prim_addFun("primFromEnumWord32", primFromEnumWord32);
  prim_addFun("primToEnumWord32", primToEnumWord32);
  prim_addFun("primWord32ToInteger", primWord32ToInteger);
  prim_addFun("primRemWord32", primRemWord32);
  prim_addFun("primQuotWord32", primQuotWord32);
  prim_addFun("primWord32FromInteger", primWord32FromInteger);
  prim_addFun("primSignumWord32", primSignumWord32);
  prim_addFun("primAbsWord32", primAbsWord32);
  prim_addFun("primMulWord32", primMulWord32);
  prim_addFun("primSubWord32", primSubWord32);
  prim_addFun("primAddWord32", primAddWord32);
  prim_addFun("primGeWord32", primGeWord32);
  prim_addFun("primGtWord32", primGtWord32);
  prim_addFun("primLeWord32", primLeWord32);
  prim_addFun("primLtWord32", primLtWord32);
  prim_addFun("primEqWord32", primEqWord32);
  prim_addFun("primFromEnumWord16", primFromEnumWord16);
  prim_addFun("primToEnumWord16", primToEnumWord16);
  prim_addFun("primWord16ToInteger", primWord16ToInteger);
  prim_addFun("primRemWord16", primRemWord16);
  prim_addFun("primQuotWord16", primQuotWord16);
  prim_addFun("primWord16FromInteger", primWord16FromInteger);
  prim_addFun("primSignumWord16", primSignumWord16);
  prim_addFun("primAbsWord16", primAbsWord16);
  prim_addFun("primMulWord16", primMulWord16);
  prim_addFun("primSubWord16", primSubWord16);
  prim_addFun("primAddWord16", primAddWord16);
  prim_addFun("primGeWord16", primGeWord16);
  prim_addFun("primGtWord16", primGtWord16);
  prim_addFun("primLeWord16", primLeWord16);
  prim_addFun("primLtWord16", primLtWord16);
  prim_addFun("primEqWord16", primEqWord16);
  prim_addFun("primFromEnumWord8", primFromEnumWord8);
  prim_addFun("primToEnumWord8", primToEnumWord8);
  prim_addFun("primWord8ToInteger", primWord8ToInteger);
  prim_addFun("primRemWord8", primRemWord8);
  prim_addFun("primQuotWord8", primQuotWord8);
  prim_addFun("primWord8FromInteger", primWord8FromInteger);
  prim_addFun("primSignumWord8", primSignumWord8);
  prim_addFun("primAbsWord8", primAbsWord8);
  prim_addFun("primMulWord8", primMulWord8);
  prim_addFun("primSubWord8", primSubWord8);
  prim_addFun("primAddWord8", primAddWord8);
  prim_addFun("primGeWord8", primGeWord8);
  prim_addFun("primGtWord8", primGtWord8);
  prim_addFun("primLeWord8", primLeWord8);
  prim_addFun("primLtWord8", primLtWord8);
  prim_addFun("primEqWord8", primEqWord8);

  prim_addFun("primReadCharAtAddr", primReadCharAtAddr);
  prim_addFun("primWriteCharAtAddr", primWriteCharAtAddr);
  prim_addFun("primReadIntAtAddr", primReadIntAtAddr);
  prim_addFun("primWriteIntAtAddr", primWriteIntAtAddr);
  prim_addFun("primReadAddrAtAddr", primReadAddrAtAddr);
  prim_addFun("primWriteAddrAtAddr", primWriteAddrAtAddr);
  prim_addFun("primReadFloatAtAddr", primReadFloatAtAddr);
  prim_addFun("primWriteFloatAtAddr", primWriteFloatAtAddr);
  prim_addFun("primReadDoubleAtAddr", primReadDoubleAtAddr);
  prim_addFun("primWriteDoubleAtAddr", primWriteDoubleAtAddr);
  prim_addFun("primReadWord8AtAddr", primReadWord8AtAddr);
  prim_addFun("primWriteWord8AtAddr", primWriteWord8AtAddr);
  prim_addFun("primReadWord16AtAddr", primReadWord16AtAddr);
  prim_addFun("primWriteWord16AtAddr", primWriteWord16AtAddr);
  prim_addFun("primReadWord32AtAddr", primReadWord32AtAddr);
  prim_addFun("primWriteWord32AtAddr", primWriteWord32AtAddr);
  prim_addFun("primReadWord64AtAddr", primReadWord64AtAddr);
  prim_addFun("primWriteWord64AtAddr", primWriteWord64AtAddr);
  prim_addFun("primReadInt8AtAddr", primReadInt8AtAddr);
  prim_addFun("primWriteInt8AtAddr", primWriteInt8AtAddr);
  prim_addFun("primReadInt16AtAddr", primReadInt16AtAddr);
  prim_addFun("primWriteInt16AtAddr", primWriteInt16AtAddr);
  prim_addFun("primReadInt32AtAddr", primReadInt32AtAddr);
  prim_addFun("primWriteInt32AtAddr", primWriteInt32AtAddr);
  prim_addFun("primReadInt64AtAddr", primReadInt64AtAddr);
  prim_addFun("primWriteInt64AtAddr", primWriteInt64AtAddr);

  prim_addFun("primFree", primFree);
  prim_addFun("primRealloc", primRealloc);
  prim_addFun("primMalloc", primMalloc);
  prim_addFun("primFinalizerFree", primFinalizerFree);

  prim_addFun("primMemcpy", primMemcpy);
  prim_addFun("primMemmove", primMemmove);
}
