#ifndef __foreign_h__
#define __foreign_h__

#include "node.h"

/** a foreign object finalizer callback. Takes the pointer to the data that
   should be freed. */
typedef void (*FinalizerCallback)(void*);

/** a finalizer link. Simply represents a linked list of finalizers */
typedef struct _ForeignPtrFinalizer {
  FinalizerCallback             callback;
  struct _ForeignPtrFinalizer*  next;
}ForeignPtrFinalizer;

/** the actual data contained within a foreign ptr.
    @field ptr     The data that this foreignPtr wraps around.
    @field marked  Whether this foreignPtr was marked.
    @field finals  A linked list of finalizers.
    @field next    The next foreignPtr in the chain 
    @field prev    The previous foreignPtr in the chain */
typedef struct _ForeignPtr {
  void*                 ptr;
  Bool                  marked;
  ForeignPtrFinalizer*  finals;
  struct _ForeignPtr*   next;
  struct _ForeignPtr*   prev;
}ForeignPtr;

/* functions */
Node*           foreign_create(void* ptr);
void            foreign_attach(FinalizerCallback callback, ForeignPtr* fp);
void            foreign_free(ForeignPtr* fp);
void            foreign_scan();
void            foreign_freeAll();

/* a handy macros */
#define FOREIGN_DEREF(fp) ((fp)->ptr)
#define FOREIGN_MARK(fp) ((fp)->marked = true)

#endif
