#include "hsffi.h"
#include "thread.h"
#include "external.h"

#define _DEBUG_ 0

#if _DEBUG_
# define SHOW(x) x
#else
# define SHOW(x)
#endif

/* the list of all free contexts */
static FFIContext* G_freeContexts = NULL;

/* the list of all waiting contexts */
static FFIContext* G_waitingContexts = NULL;

/* the FFIContext mutex, used to lock the waiting list */
static Mutex G_ffiLock = NULL;

/* the FFI waiting semaphore, used for waiting the main thread */
static Semaphore G_ffiWait = NULL;

/* the FFI context used for all 'fast' calls */
static FFIContext G_fastContext;

/*---------------------------------------------------------------------------------------------*/
static FFIContext* hsffi_allocContext();
static void hsffi_evalContext(FFIContext* ctxt);
static void hsffi_initArg(FFIArgType type, Node* argPtr, void* result);
static Node* hsffi_makeResult(FFIArgType type, void* result);
static FFICallConv hsffi_parseCallingConv(Char c);
static FFIArgType hsffi_parseArgType(Char c);
#ifndef NO_LIBFFI
    static ffi_type* hsffi_ffiArgType(FFIArgType type, Bool allowUnit);
#endif
/*---------------------------------------------------------------------------------------------*/

/* initialize the ffi system */
void hsffi_init(){
  Int i;

  G_ffiLock = mutex_create();
  G_ffiWait = yhi_semaphore_create(0);

  /* initialize arg pointers in the fast context */
  for (i = 0; i < FFI_MAX_ARGS; i++){
    G_fastContext.argPtrs[i] = &G_fastContext.argSpaces[i];
  }
}

/* allocate a new context, can only be called from the main thread */
static FFIContext* hsffi_allocContext(){
  FFIContext* ret;
  Int i;

  if (G_freeContexts){
    ret = G_freeContexts;
    G_freeContexts = ret->next;
  }else{
    ret = (FFIContext*)malloc(sizeof(FFIContext));
    for (i = 0; i < FFI_MAX_ARGS; i++){
      ret->argPtrs[i] = &ret->argSpaces[i];
    }
  }
  return ret;
}

/* load an external and return the FFIFunction for it */
FFIFunction* hsffi_loadExternal(Module* mod, Char* cname, Int arity, Char cConv, Char rType, Char* aTypes, void* funcPtr){
  FFIFunction* ret;
# ifndef NO_LIBFFI
    ffi_type* ffiRetType;
# endif
  Int i;

  ret = (FFIFunction*)malloc(sizeof(FFIFunction));
  ret->callConv = hsffi_parseCallingConv(cConv);

  /* fix for functions with single unit argument */
  if (arity == 1 && aTypes[0] == 'U'){
    arity = 0;
  }
  ret->numArgs = arity;
  ret->retType = hsffi_parseArgType(rType);
  for (i = 0; i < arity; i++){
    ret->argTypes[i] = hsffi_parseArgType(aTypes[i]);
#   ifndef NO_LIBFFI
        ret->ffiArgTypes[i] = hsffi_ffiArgType(ret->argTypes[i], false);
#   endif
  }
  /* load external from file if necessary */
  if (funcPtr){
    ret->funcPtr = funcPtr;
  }else{
    if (ret->callConv != CC_CAST){
      ret->funcPtr = ext_load(mod, cname);
    }else{
      /* don't load anything for a cast .. */
      ret->funcPtr = NULL;
    }
  }
  /* initialize libffi CIF */
# ifndef NO_LIBFFI
    ffiRetType = hsffi_ffiArgType(ret->retType, true);
    if (ffi_prep_cif(&ret->cif, FFI_DEFAULT_ABI, arity, ffiRetType, ret->ffiArgTypes) != FFI_OK){
        fprintf(stderr, "ERROR: preparing libffi cif for function '%s'\n", cname);
        abort();
    }
# endif
  return ret;
}

/* call an external function use the FFIFunction and current evaluated node, should return
   either the value to put on the stack or NULL if the call was delayed */
Node* hsffi_call(FFIFunction* ffiFunc, Node* node){
# ifdef NO_LIBFFI
    PrimitiveFunc pf = ffiFunc->funcPtr;
    FFIContext* ctxt;
    switch (ffiFunc->callConv){
    case CC_ADDRESS:
        return hsffi_makeResult(A_PTR, (void*)&ffiFunc->funcPtr);
    case CC_CAST:
        ctxt = &G_fastContext;
        hsffi_initArg(ffiFunc->argTypes[0], node->args[0], &ctxt->retSpace);
        return hsffi_makeResult(ffiFunc->retType, &ctxt->retSpace);
    default:
        pf = ffiFunc->funcPtr;
        return pf(node);
    }
# else
    PrimitiveFunc pf;
    FFIContext* ctxt;
    Bool fast;
    Int i;

    /* do different things for different calling conventions */
    switch (ffiFunc->callConv){
    case CC_ADDRESS:
        return hsffi_makeResult(A_PTR, (void*)&ffiFunc->funcPtr);
    case CC_PRIMITIVE:
        pf = ffiFunc->funcPtr;
        return pf(node);
    case CC_CAST: {
        ctxt = &G_fastContext;
        hsffi_initArg(ffiFunc->argTypes[0], node->args[0], &ctxt->retSpace);
        return hsffi_makeResult(ffiFunc->retType, &ctxt->retSpace);
    }
    case CC_FAST_C:
    case CC_FAST_STD:
        fast = true;
        break;
    case CC_C:
    case CC_STD:
        fast = proc_isOnlyProcess();
        break;
    default:
        abort(); /* erm ... don't know this calling convention and it should have been picked up earlier */
        return NULL;
    }
    /* choose the right context */
    if (fast){
        ctxt = &G_fastContext;
    }else{
        ctxt = hsffi_allocContext();
    }
    /* set context details */
    ctxt->func = ffiFunc;
    ctxt->parent = G_proc;
    ctxt->waiters = NULL;
    G_proc->waitContext = ctxt;

    /* initialize the arguments */
    for (i = 0; i < ffiFunc->numArgs; i++){
        hsffi_initArg(ffiFunc->argTypes[i], node->args[i], ctxt->argPtrs[i]);
    }
    /* make the call */
    if (fast){
        /* just call it directly */
        ffi_call(&ffiFunc->cif, ffiFunc->funcPtr, &ctxt->retSpace, ctxt->argPtrs);
        return hsffi_makeResult(ffiFunc->retType, &ctxt->retSpace);
    }else{
        /* stick it in a thread */
        yhi_thread_create((ThreadProc)hsffi_evalContext, ctxt);
        SHOW(printf("hsffi: Process %d is waiting on context %p\n", G_proc->id, ctxt));
        /* reschedule the processes */
        proc_switch(PM_WAITING);
        return NULL;
    }
#endif
}

/* evaluate a context and setup the result, used when evaluating ffi in a thread */
static void hsffi_evalContext(FFIContext* ctxt){
#ifndef NO_LIBFFI
  FFIFunction* func = ctxt->func;

  /* evaluate function */
  ffi_call(&func->cif, func->funcPtr, &ctxt->retSpace, ctxt->argPtrs);

  /* put us on the waiting list */
  mutex_lock(G_ffiLock);
  ctxt->next = G_waitingContexts;
  SHOW(printf("hsffi: Context %p is put on waiting list (waited on by process %d)\n", ctxt, ctxt->parent->id));
  G_waitingContexts = ctxt;
  yhi_semaphore_signal(G_ffiWait);
  mutex_unlock(G_ffiLock);
#endif
}

/* finalize all waiting contexts:
   must be called from main thread because it can allocate heap */
void hsffi_finalizeAllWaiting(){
  FFIContext* c;
  FFIContext* next;

  mutex_lock(G_ffiLock);
  for (c = G_waitingContexts; c; c = next){
    /* get result */
    Node* ret = hsffi_makeResult(c->func->retType, &c->retSpace);

    /* move context to free list */
    next = c->next;
    c->next = G_freeContexts;
    G_freeContexts = c;

    /* if the process has waiters make the waiters ready */
    if (c->waiters){
        Process* p;
        Process* next;

        for (p = c->waiters; p; p = next){
            next = p->next;

            /* need to put the waiters on ultra high priority so that some throw will definitely run.
               If not we can get into the tricky situation that A runs FFI, B throws to A and blocks,
               C throws to B and at the same time A completes and makes B ready. If C then runs before B
               then A is left hanging and dead, however if B runs before C then it's just as if C was too
               late to prevent B throwing and no problem occours. Thus FFI waiters always take priority */
            proc_makeReadyHiPriority(p);
        }
    }

    /* give process result, this is a good idea even if we have waiters. Apart from anything else we
       need to make the process ready for a THROW_TO to run successfully */
    SHOW(printf("hsffi: context %p gives a result to process %d\n", c, c->parent->id));

    assert(c->parent->waitContext == c);
    c->parent->waitContext = NULL;
    proc_ffiResult(c->parent, ret);
  }
  /* clear the wait list */
  G_waitingContexts = NULL;

  /* zero the semaphore */
  yhi_semaphore_zero(G_ffiWait);
  mutex_unlock(G_ffiLock);
}

/* request that a FFI call never try and return a result, for example because the parent process
   has had an exception thrown to it */
void hsffi_waitOnContext(FFIContext* ctxt){
  /* add the current process onto the list of waiters */
  G_proc->next = ctxt->waiters;
  ctxt->waiters = G_proc;
  G_proc->waitContext = ctxt;
}

/* remove a process from the list of waiters */
void hsffi_removeWaiter(FFIContext* ctxt, Process* waiter){
    Process* p;
    Process* prev = NULL;

    /* scan all waiting processes */
    for (p = ctxt->waiters; p != NULL; p = p->next){
        if (p == waiter){
            if (prev){
                prev->next = p->next;
            }else{
                ctxt->waiters = p->next;
            }
            return;
        }
        prev = p;
    }
    /* should never reach here */
    abort();
}

/* wait for some FFI action to complete:
   this is called by the main thread when it's got nothing to do but wait for an FFI action */
void hsffi_waitForFFI(){
  yhi_semaphore_wait(G_ffiWait);
}

/* initialize an ffi argument */
static void hsffi_initArg(FFIArgType type, Node* node, void* result){
  REMOVE_IND(node, Node*);
  switch (type){
  case A_PTR: *(void**)result = UNBOX_PTR(node); break;
  case A_FOREIGN_PTR: *(void**)result = UNBOX_FOREIGN_PTR(node); break;
  case A_CHAR: *(Char*)result = UNBOX_CHAR(node); break;
  case A_INT: *(Int*)result = UNBOX_INT(node); break;
  case A_BOOL: *(Bool*)result = UNBOX_BOOL(node); break;
  case A_FLOAT: *(Float*)result = UNBOX_FLOAT(node); break;
  case A_DOUBLE: *(Double*)result = UNBOX_DOUBLE(node); break;
  case A_INT8: *(Int8*)result = UNBOX_INT8(node); break;
  case A_INT16: *(Int16*)result = UNBOX_INT16(node); break;
  case A_INT32: *(Int32*)result = UNBOX_INT32(node); break;
  case A_INT64: *(Int64*)result = UNBOX_INT64(node); break;
  case A_WORD8: *(UInt8*)result = UNBOX_WORD8(node); break;
  case A_WORD16: *(UInt16*)result = UNBOX_WORD16(node); break;
  case A_WORD32: *(UInt32*)result = UNBOX_WORD32(node); break;
  case A_WORD64: *(UInt64*)result = UNBOX_WORD64(node); break;
  case A_UNIT: break;
  case A_UNKNOWN: *(Node**)result = UNBOX_UNKNOWN(node); break;
  default:
    abort();
  }
}

/* make a FFI result */
static Node* hsffi_makeResult(FFIArgType type, void* result){
  Node* ret;

  switch (type){
  case A_PTR: BOX_PTR(ret, *(void**)result); break;
  case A_CHAR: BOX_CHAR(ret, *(Char*)result); break;
  case A_INT: BOX_INT(ret, *(Int*)result); break;
  case A_BOOL: BOX_BOOL(ret, *(Bool*)result); break;
  case A_FLOAT: BOX_FLOAT(ret, *(Float*)result); break;
  case A_DOUBLE: BOX_DOUBLE(ret, *(Double*)result); break;
  case A_INT8: BOX_INT8(ret, *(Int8*)result); break;
  case A_INT16: BOX_INT16(ret, *(Int16*)result); break;
  case A_INT32: BOX_INT32(ret, *(Int32*)result); break;
  case A_INT64: BOX_INT64(ret, *(Int64*)result); break;
  case A_WORD8: BOX_WORD8(ret, *(UInt8*)result); break;
  case A_WORD16: BOX_WORD16(ret, *(UInt16*)result); break;
  case A_WORD32: BOX_WORD32(ret, *(UInt32*)result); break;
  case A_WORD64: BOX_WORD64(ret, *(UInt64*)result); break;
  case A_UNIT: ret = NODE_UNIT; break;
  case A_UNKNOWN: BOX_UNKNOWN(ret, *(Node**)result); break;
  default:
    abort();
  }
  return ret;
}

/* parse a calling convention */
static FFICallConv hsffi_parseCallingConv(Char c){
  switch (c){
  case 'c': return CC_C;
  case 's': return CC_STD;
  case 'C': return CC_FAST_C;
  case 'S': return CC_FAST_STD;
  case 'x': return CC_CAST;
  case 'p': return CC_PRIMITIVE;
  case 'a': return CC_ADDRESS;
  default:
    fprintf(stderr, "ERROR: unknown calling convention '%c'\n", c);
    exit(1);
  }
}

/* parse an argument type */
static FFIArgType hsffi_parseArgType(Char c){
  switch (c){
  case 'P': return A_PTR;
  case 'C': return A_CHAR;
  case 'I': return A_INT;
  case 'B': return A_BOOL;
  case 'F': return A_FLOAT;
  case 'D': return A_DOUBLE;
  case 'i': return A_INT8;
  case 'j': return A_INT16;
  case 'k': return A_INT32;
  case 'l': return A_INT64;
  case 'w': return A_WORD8;
  case 'x': return A_WORD16;
  case 'y': return A_WORD32;
  case 'z': return A_WORD64;
  case 'f': return A_FOREIGN_PTR;
  case 'S': return A_PTR;
  case 'U': return A_UNIT;
  case 'N': return A_UNKNOWN; /* Integer - can't do much else with it really .. */
  case 'u': return A_UNKNOWN;
  default:
    fprintf(stderr, "ERROR: unknown or unsupported argument type '%c'\n", c);
    exit(1);
  }
}

/* return the libffi type for a FFIArgType */
#ifndef NO_LIBFFI
static ffi_type* hsffi_ffiArgType(FFIArgType type, Bool allowUnit){
  ffi_type* type_int;

  /* this is somewhat of a nasty hack ... */
#if (WORD_BITS == 32)
  type_int = &ffi_type_sint32;
#elif (WORD_BITS == 64)
  type_int = &ffi_type_sint64;
#else
# error "word size is neither 32 nor 64 bits"
#endif

  switch (type){
  case A_PTR: return &ffi_type_pointer;
  case A_FOREIGN_PTR: return &ffi_type_pointer;
  case A_CHAR: return &ffi_type_sint8;
  case A_INT: return type_int;
  case A_BOOL: return type_int;
  case A_FLOAT: return &ffi_type_float;
  case A_DOUBLE: return &ffi_type_double;
  case A_INT8: return &ffi_type_sint8;
  case A_INT16: return &ffi_type_sint16;
  case A_INT32: return &ffi_type_sint32;
  case A_INT64: return &ffi_type_sint64;
  case A_WORD8: return &ffi_type_uint8;
  case A_WORD16: return &ffi_type_uint16;
  case A_WORD32: return &ffi_type_uint32;
  case A_WORD64: return &ffi_type_uint64;
  case A_UNKNOWN: return &ffi_type_pointer;
  case A_UNIT:
    if (allowUnit){
      return &ffi_type_void;
    }
    fprintf(stderr, "ERROR: hsffi_ffiArgType: tried to pass 'unit' arg!\n");
    exit(1);
    break;
  default:
    abort();
  }
}
#endif

