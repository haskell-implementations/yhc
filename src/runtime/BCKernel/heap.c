#include "heap.h"
#include "sanity.h"
#include "stopcopy.h"
#include "foreign.h"

#define DEBUG 1

#if DEBUG
# define SHOW(x) x
#else
# define SHOW(x)
#endif

#define ASSERT(x) assert(x)

UInt   G_markTableSize = 0;
Word*  G_markTable = NULL;

Bool  G_gcEnabled = false;
UInt  G_hpSize = 0;
Word* G_hp = NULL;
Word* G_hpStart = NULL;
Word* G_hpEnd = NULL;

UInt   G_reserve = 0;
Node** G_sp = NULL;
Node** G_spBase = NULL;
Node** G_spLimit = NULL;
Frame* G_fp = NULL;

void heap_init(Int heapSize){
  Int wSize  = heapSize / sizeof(Word);

  G_hpSize = wSize;

  G_markTableSize = (wSize / WORD_BITS) + (((wSize % WORD_BITS) == 0) ? 0 : 1);
  G_markTable = (Word*)malloc(G_markTableSize*sizeof(Word));

  G_hpStart = (Word*)malloc(wSize * sizeof(Word));
  G_hp = G_hpStart;
  G_hpEnd = G_hpStart + wSize;

  G_spBase = G_spLimit = G_sp = NULL;
  G_fp = NULL;
}

void heap_exit(){
  /* run all foreign finalizers */
  foreign_freeAll();

  /* free the heap */
  free(G_hpStart);

  /* print any necessary starts */
#if 0
  printf("=== GC statistics ================================\n");
  printf("  num GCs:\t%ld\n", G_gcStats.numGCs);
  printf("  words used:\t%Lu\n", G_gcStats.wordsUsed);
  printf("  words moved:\t%Lu\n", G_gcStats.wordsMoved);
  printf("  max live:\t%ld\n", G_gcStats.maxLive);
  printf("  retention:\t%g%%\n", (Double)G_gcStats.wordsMoved / (Double)G_gcStats.wordsUsed * 100.0);
  printf("==================================================\n");
#endif

  /* reset all variables */
  G_hpStart = G_hp = G_hpEnd = NULL;
  G_sp = G_spBase = NULL;
  G_fp = NULL;
}

Word* heap_alloc(UInt size){
  Word* ret;
  if (G_hp + size >= G_hpEnd){
    heap_gc(size);
  }
  ret = G_hp;
  G_hp += size;
  return (Word*)ret;
}

void jonk_collect();

void heap_gc(UInt size){
  if (!G_gcEnabled){
    fprintf(stderr, "Tried to garbage collect with collection disabled!\n");
    fprintf(stderr, "\tThis probably means that we ran out of memory loading the bytecode file\n");
    fprintf(stderr, "\tPlease set a larger initial heap size\n");
    exit(1);
  }
  /*  fprintf(stderr,  "Ran out of memory!\n");
      abort(); */
/*     printf("################# Garbage collection! ##############################################\n"); */
  jonk_collect();

  /* run any foreign finalizers */
  foreign_scan();

  /*  sanity_heap(true); */

  if (G_hp + size >= (Word*)G_hpEnd){
    fprintf(stderr, "Ran out of memory!\n");
    fprintf(stderr, "\tTried to get %ld words, after GC there were only %ld available\n",size,
                (long)((Word*)G_hpEnd-G_hp));
    fprintf(stderr, "\tNeed to set a bigger heap size ...\n");
    exit(1);
  }
}

/*--------------------------------------------------------------------------------------------*/

/* the global variable handles */
Global*     G_heapGlobals = NULL;

/* add a new global variable */
void heap_pushGlobal(Global* g){
  g->next = G_heapGlobals;
  G_heapGlobals = g;
}

/* remove the top global variable */
void heap_popGlobal(){
  G_heapGlobals = G_heapGlobals->next;
}

