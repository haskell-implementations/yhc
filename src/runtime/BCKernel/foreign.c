/* ForeignPtr code.
   
   This is complicated by interaction with the garbage collector. The idea is that
   we allocate ForeignPtr in normal heap and a ForeignPtrNode in the haskell heap.
   After every mark we then scan all the foreignPtrs to see whether any have become
   unmarked. We unlink the unmarked ones and run all the finalizers.
   
   Garbage collection will then move all the ForeignPtrNodes in the haskell heap but
   there internal pointers will still point to the same C heap allocated ForeignPtr
   structures.   
*/

#include "node.h"
#include "heap.h"
#include "primitive.h"
#include "foreign.h"

/** the list of foreignPtrs. This is needed so that after mark we can scan all the
    foriegn ptrs to see if any were not marked */
static ForeignPtr* G_fpFirst = NULL;

/** create a new, blank, foreign ptr.
    @param    ptr the data this foreign ptr wraps
    @returns      a pointer to the allocated heap node */
Node* foreign_create(void* ptr){
  ForeignPtr* fp;
  ForeignPtrNode* fpnode;

  /* allocate the foreign ptr */
  fp = (ForeignPtr*)malloc(sizeof(ForeignPtr));
  fp->ptr = ptr;
  fp->marked = false;
  fp->finals = NULL;

  /* link into the chain */
  fp->next = G_fpFirst;
  fp->prev = NULL;
  if (G_fpFirst){
    G_fpFirst->prev = fp;
  }
  G_fpFirst = fp;

  /* now allocate the wrapper node */
  fpnode = (ForeignPtrNode*)heap_alloc(wordsof(ForeignPtrNode));
  MAKE_NODE(fpnode, &G_infoForeignPtr, N_NORMAL);
  fpnode->ptr = fp;
  return (Node*)fpnode;
}

/** attach a finalizer callback to a foreign ptr object. This adds the callback
    to the beginning of the list so that it will be run first.
    @param final the finalizer callback
    @param fp    the foreignPtr object */    
void foreign_attach(FinalizerCallback final, ForeignPtr* fp){
  ForeignPtrFinalizer* fin;

  fin = (ForeignPtrFinalizer*)malloc(sizeof(ForeignPtrFinalizer));
  fin->callback = final;
  fin->next = fp->finals;
  fp->finals = fin;
}

/** actually frees a foreign object. This runs all the finalizers and
    unlinks from the list of foreign objects */
void foreign_free(ForeignPtr* fp){
  ForeignPtrFinalizer* ff;
  ForeignPtrFinalizer* next;

  /* run all the finalizers */
  for (ff = fp->finals; ff != NULL; ff = next){
    ff->callback(fp->ptr);
    next = ff->next;
    free(ff);
  }

  /* unlink from the list */
  if (fp->prev){
    fp->prev->next = fp->next;
  }else{
    G_fpFirst = fp->next;
  }
  if (fp->next){
    fp->next->prev = fp->prev;
  }
  fp->prev = fp->next = NULL;

  /* free the foreign ptr data */
  free(fp);
}

/** This scans all the foreign ptrs that are registered to find those that are
    not marked and free them. */
void foreign_scan(){
  ForeignPtr* fp;
  ForeignPtr* next;

  for (fp = G_fpFirst; fp != NULL; fp = next){
    next = fp->next;
    if (fp->marked){
      fp->marked = false;
      continue;
    }
    foreign_free(fp);
  }
}

/** run all finalizers and free all foreign ptr objects */
void foreign_freeAll(){
  ForeignPtr* fp;
  ForeignPtr* next;

  for (fp = G_fpFirst; fp != NULL; fp = next){
    next = fp->next;
    foreign_free(fp);
  }
}
