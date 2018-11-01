#ifndef __heap_h__
#define __heap_h__

#include "node.h"

/* a frame is the part of the stack that records successive applications,
   it is stored as a structure making additions and the like very easy */
typedef struct _Frame {
  struct _Frame*   fp;
  CodePtr          ip;
  Node*            vapptr;
}Frame;

/* the size of a frame in heap words */
#define FRAME_SIZE  wordsof(Frame)
#define HEAP_OFFS(p) ((Word*)(p) - (Word*)G_hpStart)

/* global heap constants */
extern UInt   G_markTableSize;
extern Word*  G_markTable;

extern Bool  G_gcEnabled;
extern UInt  G_hpSize;
extern Word* G_hp;
extern Word* G_hpStart;
extern Word* G_hpEnd;

extern UInt   G_reserve;
extern Node** G_sp;
extern Node** G_spBase;
extern Node** G_spLimit;
extern Frame* G_fp;

/* heap manipulation functions */
void  heap_init(Int heapSize);
void  heap_exit();
Word* heap_alloc(UInt words);
void  heap_gc(UInt size);


typedef struct _GCStats {
  Int    numGCs;
  UInt64 wordsUsed;
  UInt64 wordsMoved;
  Int    maxLive;
}GCStats;

extern GCStats G_gcStats;


/* 'global variable's are used to hold pointers from C into the Haskell heap.
   specifically these are organised as a stack. Generally used to hold values inside
   primitive functions */
typedef struct _Global {
  struct _Global*  next;
  Node**           global;
}Global;

void heap_pushGlobal(Global* glob);
void heap_popGlobal();

/* all the global variables */
extern Global*     G_heapGlobals;

/*               Heap paging is currently not used               */
#if 0
/* a page in the heap represents a block of continous memory */
typedef struct _Page {
  struct _Page*   next;           /* link to the next block */
  UInt            size;           /* how large the block is */
  Node**          saveSp;         /* used to store the previous stack pointer for stack pages */
  Frame*          saveFp;         /* saved frame pointer */
  Word            data[0];        /* the data in the page */
}Page;

/*
extern UInt    G_pageSize;
extern UInt    G_totalAlloced;
extern Bool    G_gcEnabled;
extern Page*   G_livePages;

extern Page*   G_heapPages;
extern Page*   G_heap;
extern Word*   G_hp;
extern Word*   G_hpLimit;

extern Page*   G_stackPages;
extern Page*   G_stack;
extern Node**  G_spBase;
extern Node**  G_sp;
extern Node**  G_spLimit;
extern Frame*  G_fp;

void  heap_init();
void  heap_exit();

Word*  heap_alloc(UInt size);
void   heap_reserve(UInt size);
void   stack_reserve(UInt size);
void   stack_release();
*/
#endif


#endif
