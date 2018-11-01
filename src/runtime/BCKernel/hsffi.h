#ifndef __hsffi_h__
#define __hsffi_h__

#include "process.h"
#include "module.h"
#include "primitive.h"
#include "foreign.h"

#define FFI_MAX_ARGS          64

/* the various possible argument types */
typedef enum { A_PTR, A_CHAR, A_INT, A_LONG, A_BOOL, A_FLOAT, A_DOUBLE,
               A_INT8, A_INT16, A_INT32, A_INT64, A_WORD8, A_WORD16, A_WORD32, A_WORD64,
               A_FOREIGN_PTR, A_UNIT, A_UNKNOWN } FFIArgType;

typedef Node* (*PrimitiveFunc)(Node*);

/* the various possible calling conventions */
typedef enum { CC_C, CC_STD, CC_FAST_C, CC_FAST_STD,
               CC_CAST, CC_PRIMITIVE, CC_ADDRESS } FFICallConv;

/* information about a FFI function */
typedef struct _FFIFunction {
  FFICallConv             callConv;
  Int                     numArgs;
  FFIArgType              argTypes[FFI_MAX_ARGS];
  FFIArgType              retType;
  void*                   funcPtr;

  /* libffi calling handle */
#ifndef NO_LIBFFI
  ffi_type*               ffiArgTypes[FFI_MAX_ARGS];
  ffi_cif                 cif;
#endif
}FFIFunction;

/* a particular call to a FFI function */
typedef struct _FFIContext {
  struct _FFIContext*     next;                             /* next ffi context in chain, used in free-lists */
  FFIFunction*            func;                             /* the ffi function we are calling */
  Process*                parent;                           /* the process that called this ffi function */
  Process*                waiters;                          /* list of processes waiting on this FFI call */
  void*                   argPtrs[FFI_MAX_ARGS];            /* pointers to all the arguments */
  UInt64                  argSpaces[FFI_MAX_ARGS];          /* the space for the arguments */
  UInt64                  retSpace;                         /* the space for returning */
}FFIContext;

void          hsffi_init();
FFIFunction*  hsffi_loadExternal(Module* mod, Char* name, Int arity, Char callConv, Char retType, Char* argTypes, void* funcPtr);
Node*         hsffi_call(FFIFunction* func, Node* node);
void          hsffi_finalizeAllWaiting();
void          hsffi_waitForFFI();
void          hsffi_waitOnContext(FFIContext* ctxt);
void          hsffi_removeWaiter(FFIContext* ctxt, Process* waiter);

/* macros to box and unbox values */
#define BOX_PTR(res,val)  \
  res = (Node*)heap_alloc(wordsof(BoxNode)); \
  MAKE_NODE(res, &G_infoBox, N_NORMAL); \
  ((BoxNode*)res)->ptr = (void*)val

#define BOX_BOOL(res,val) \
  res = (val) ? G_nodeTrue : G_nodeFalse

#define BOX_STRING(res,val) BOX_UNKNOWN(res,val)
#define BOX_INT(res,val)  \
  res = (Node*)heap_alloc(wordsof(INode)); \
  MAKE_NODE(res, &G_infoInt, N_NORMAL); \
  ((INode*)res)->value = (Int)val

#define BOX_LONG(res,val) \
  res = (Node*)heap_alloc(wordsof(LongNode)); \
  MAKE_NODE(res, &G_infoLong, N_NORMAL); \
  ((LongNode*)res)->value = (Int64)val

#define BOX_FLOAT(res,val) \
  res = (Node*)heap_alloc(wordsof(FloatNode)); \
  MAKE_NODE(res, &G_infoFloat, N_NORMAL); \
  ((FloatNode*)res)->value = (Float)val

#define BOX_DOUBLE(res,val) \
  res = (Node*)heap_alloc(wordsof(DoubleNode)); \
  MAKE_NODE(res, &G_infoDouble, N_NORMAL); \
  ((DoubleNode*)res)->value = (Double)val

#define BOX_INTEGER(res,val) \
  res = val;

#define BOX_FUN_PTR(res,val) BOX_PTR(res,val)
#define BOX_FOREIGN_PTR(res,val) res = val;

#define BOX_CHAR(res,val) BOX_INT(res,val)
#define BOX_INT8(res,val) BOX_INT(res,val)
#define BOX_INT16(res,val) BOX_INT(res,val)
#define BOX_INT32(res,val) BOX_INT(res,val)
#define BOX_INT64(res,val) BOX_LONG(res,val)

#define BOX_WORD8(res,val) BOX_INT(res,val)
#define BOX_WORD16(res,val) BOX_INT(res,val)
#define BOX_WORD32(res,val) BOX_INT(res,val)
#define BOX_WORD64(res,val) BOX_LONG(res,val)

/* #define BOX_FOREIGN_OBJ(res,val) res = val */
#define BOX_ADDR(res,val) BOX_PTR(res,val)
#define BOX_UNKNOWN(res,val) res = (Node*)val

#define UNBOX_STRING(p)  UNBOX_UNKNOWN(p)
#define UNBOX_PTR(p)     (((BoxNode*)p)->ptr)
#define UNBOX_INT(p)     (((INode*)p)->value)
#define UNBOX_LONG(p)    (((LongNode*)p)->value)
#define UNBOX_BOOL(p)    ((Node*)p == G_nodeTrue)
#define UNBOX_FLOAT(p)   (((FloatNode*)p)->value)
#define UNBOX_DOUBLE(p)  (((DoubleNode*)p)->value)

#define UNBOX_CHAR(p)    (Char)UNBOX_INT(p)
#define UNBOX_INT8(p)    (Int8)UNBOX_INT(p)
#define UNBOX_INT16(p)   (Int16)UNBOX_INT(p)
#define UNBOX_INT32(p)   (Int32)UNBOX_INT(p)
#define UNBOX_INT64(p)   (Int64)UNBOX_LONG(p)
#define UNBOX_WORD8(p)   (UInt8)UNBOX_INT(p)
#define UNBOX_WORD16(p)  (UInt16)UNBOX_INT(p)
#define UNBOX_WORD32(p)  (UInt32)UNBOX_INT(p)
#define UNBOX_WORD64(p)  (UInt64)UNBOX_LONG(p)

#define UNBOX_FOREIGN_PTR(p)  (FOREIGN_DEREF((ForeignPtr*)(UNBOX_PTR(p))))
#define UNBOX_UNKNOWN(p) p
#define UNBOX_INTEGER(p) p
#define UNBOX_ADDR(p) UNBOX_PTR(p)

#define UNBOX_FUN_PTR(p) UNBOX_PTR(p)

#define UNBOX_UNIT(p)

#define NODE_UNIT G_nodeUnit

typedef Int                 HsInt;
typedef Bool                HsBool;
typedef Int64               HsInt64;
typedef Int32               HsInt32;
typedef Int16               HsInt16;
typedef Int8                HsInt8;
typedef UInt64              HsWord64;
typedef UInt32              HsWord32;
typedef UInt16              HsWord16;
typedef UInt8               HsWord8;
typedef void*               FunPtr;

typedef void                (*WrapRegisterFun)(const char*, const char*, void*, void*);

/* ========= SHELVED ============================================================================================ */
#if 0
#define FFI_MAX_ARGS        64

/* an argument to a FFI call */
typedef struct _FFIArg {
  FFIArgType          type;
  union {
    void*             p;
    Char              c;
    Int               i;
    Bool              b;
    Int64             l;
    Float             f;
    Double            d;
    Int8              i8;
    Int16             i16;
    Int32             i32;
    Int64             i64;
    UInt8             w8;
    UInt16            w16;
    UInt32            w32;
    UInt64            w64;
  }                   value;
}FFIArg;

/* a FFIContext */
typedef struct _FFIContext {
  struct _FFIContext*   next;
  Process*              parent;
  Int                   numArgs;
  FFIArg                args[FFI_MAX_ARGS];
  FFIArg                ret;
}FFIContext;

typedef void (*FFIEvalProc)(FFIContext* ctxt);

void            ffic_init();
FFIContext*     ffic_create();
void            ffic_addArg(FFIContext* ctxt, FFIArgType type, Node* arg);
Node*           ffic_eval(FFIContext* ctxt, FFIEvalProc proc);
void            ffic_result(FFIContext* ctxt);

void            ffic_finalizeAllWaiting();
void            ffic_waitForFFI();
#endif

#endif
