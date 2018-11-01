/*------------------------------------------------------------------------------------------------------------------
  How this works
  ------------------------------------------------------------------------------------------------------------------

  this is a fairly standard 'Jonkers' collector. Essentially first the heap is transformed.
  Consider the case when nodes X, Y and Z all have a field linking to node A. This is transformed so that

    A.info = X.fieldX, X.fieldX = Y.fieldY, Y.fieldY = Z.fieldZ, Z.fieldZ = A.info

  these are the 'threaded' chains. Then when we decide to move 'A' we go along the chain and update all the references
  we find, before installing the original info back.

*/

#define HEAPOFFS(p) ((Word*)(p)-G_hpStart)

#include "heap.h"
#include "mark.h"
#include "pretty.h"
#include "primitive.h"
#include "sanity.h"
#include "integer.h"
#include "process.h"
#include "stable.h"

#define DEBUG 0

#if DEBUG
# define SHOW(x) x
#else
# define SHOW(x)
#endif

#define ASSERT(x) assert(x)

GCStats G_gcStats = { 0, 0, 0, 0 };
Int G_moveCount;

/* print a jonkers thread */
void pr_jonk_node(Node* node, Int depth){
  Word* i;

  printf("{ ");
  i = (Word*)node;
  while (((*i) & N_MASK) == N_IND){
    printf("%p ", *(Word**)i);
    i = *(Word**)i;
  }
  printf("} ");pr_nodeWith((Node*)i, node->args, 1);
}

/* link an additional item into a thread,
   p points to a cell whose contents are a pointer to a heap node.
   the heap node may be threaded already in which case this code adds a new entry */
void jonk_thread(Node** p){
  Node** k;
  SHOW(Node* node = *p);

  ASSERT(*p >= (Node*)G_hpStart && *p <= (Node*)G_hp);

  SHOW(printf("\t\tthread %p (*=%p)[%d] = ", p, *p, HEAPOFFS(*p));fflush(stdout));
  if (!mkt_isMarked(*p)){
    ASSERT(mkt_isMarked(*p));
  }

  k = (Node**)*p;
  *p = *k;
  *k = (Node*)p;

  SHOW(pr_jonk_node(node, 1); printf("\n"));
}

/* thread all the variables on the stack */
void jonk_threadStack(){
  Node** p;
  Frame* f;
  Node** start;
  Process* proc;

  SHOW(printf("Threading the stack\n"));
  /* go through every process */
  for (proc = G_procList; proc; proc = proc->linkNext){
    /* go through every frame */
    for (f = proc->saveFP; f; f = f->fp){
      /* thread all the stack variables */
      if (f->vapptr){
        jonk_thread(&f->vapptr);
      }
      start = ((Node**)f) + FRAME_SIZE;
      for (p = start; p < (Node**)f->fp; p++){
        jonk_thread(p);
      }
    }
    /* thread the processes stack link */
    jonk_thread((Node**)&proc->stack);
    /* thread the blocked on */
    jonk_thread(&proc->blockedOn);
  }
}

/* thread all node references in the CAFs */
void jonk_threadCAFs(){
  FInfo* f;
  FInfo* next;

  SHOW(printf("Threading the CAFs\n"));
  /* go through all the CAFs */
  for (f = G_firstCaf; f != NULL; f = next){
    Int i;

    /* scan the finfo const table */
    for (i = 0; i < f->numConsts; i++){
      if (f->constTypes[i] == C_NODE){
        jonk_thread((Node**)&f->constTable[i]);
      }
    }
    next = f->link;
    /* restore the finfo link pointers */
    f->link = NULL;
  }
}

/* thread all our global variables */
void jonk_threadGlobals(){
  Global* p;

  SHOW(printf("Threading the globals\n"));
  for (p = G_heapGlobals; p; p = p->next){
    jonk_thread(p->global);
  }
}

/* thread all the stable pointers */
void jonk_threadStablePtrs(){
  StablePtr* p;

  SHOW(printf("Threading the stable pointers\n"));
  for (p = G_stablePtrs; p; p = p->next){
    jonk_thread(&p->node);
  }
}

/* update a heap node with a new position,
   this scans down the update chain and updates them all */
void jonk_update(Node* item, Word* new){
  Word* old  = (Word*)item;
  Word p = *old;
  while ((p & N_MASK) == N_IND){
    Word q = *(Word*)p;
    SHOW(printf("\t\tupdate %p with %p[%d]\n", (Word*)p, new, HEAPOFFS(new)));
    *(Word*)p = (Word)new;
    p = q;
  }
  *old = p;
}

/* scan the heap, threading and updating */
void jonk_scan(){
  Node* p;
  Word* new = G_hpStart;

  SHOW(printf("SCANNING %p to %p[%d], sp = %p, end of heap  = %p\n", G_hpStart, G_hp, HEAPOFFS(G_hp),
       G_sp, G_hpEnd));

  /* scan the heap */
  for (p = (Node*)G_hpStart; p < (Node*)G_hp;){
    Node* newP;
    UInt size = 0;
    Int i;
    UInt bSize;
    Int fstArg, lstArg;
    Bool hasPtrs = true;

    SHOW(printf("\tChecking %p[%d]\n", p, HEAPOFFS(p)));
    /* if p isn't marked ... */
    newP = mkt_firstMarked(p);
    if (newP != p){
      /* save the new position in p so that the move phase will be quicker */
      MAKE_NODE(p, newP, N_GC);
      p = newP;
    }
    /* maybe we reached the end ... */
    if (p >= (Node*)G_hp){
      break;
    }
    SHOW(printf("\tUpdating %p[%d] [", p, HEAPOFFS(p));pr_jonk_node(p,1);printf("] to %p[%d]\n",new,
         HEAPOFFS(new)));
    /* this node is definitely marked so do jonkers stuff on it */
    jonk_update(p, new);

    size = node_size(p, &fstArg, &lstArg, &hasPtrs);
    for (i = fstArg; hasPtrs && i <= lstArg; i++){
      jonk_thread(p->args + i);
    }
    bSize = wordsof(Node) + size;
    new += bSize;
    /* advance p the correct number of words */
    p = (Node*)(((Word*)p) + bSize);
  }

  SHOW(printf("SCAN finished p = %p[%d], hp = %p[%d]\n", p, HEAPOFFS(p), G_hp, HEAPOFFS(G_hp)));
}

/* move an item 'old', of size 'size' to position 'new' */
void jonk_moveOne(Node* old, Int size, Word* new){
  Word* p = new;
  Word* q = (Word*)old;
  Int i;
  Info* info;

  G_moveCount++;

  SHOW(printf("\t\tmoving %ld words at %p[%d] to %p[%d]\n", size, old, HEAPOFFS(old), new, HEAPOFFS(new)));

  /* check whether this is a proc stack node */
  info = NODE_INFO(old);
  /* NOTE: don't need to derefence TInfos, since we are checking for a CInfo */
  if (info->tag == I_CINFO && ((CInfo*)info)->flags & CI_PROC_STACK){
    proc_moveStack((ProcStackNode*)old, (ProcStackNode*)new, true);
  }else{
    /* FIXME: isn't this what memcpy is for?
       REPLY: probably not, these are all small and the function call is probably expensive ... */
    for (i = 0; i < size; i++){
      *p++ = *q++;
    }
  }
}

/* go through the heap and move all the entries to their new positions */
void jonk_move(){
  Node* p;
  Word* new = G_hpStart;

  SHOW(printf("MOVING from %p to %p\n", G_hpStart, G_hp));

  /* scan the whole heap */
  for (p = (Node*)G_hpStart; p < (Node*)G_hp;){
    Int size, wSize;
    Bool hasPtrs;
    Int fstArg, lstArg;

    /* skip over any blank spaces */
    if (NODE_FLAGS(p) == N_GC){
      Node* q = (Node*)NODE_INFO(p);
      p = q;
    }
    /* we might have skipped to the end of the heap ... */
    if (p >= (Node*)G_hp){
      break;
    }
    ASSERT(NODE_FLAGS(p) != N_GC);
    SHOW(printf("\tMoving %p[%d] [", p, HEAPOFFS(p));pr_jonk_node(p,1);printf("] to %p[%d]\n", new,
         HEAPOFFS(new)));
    /* update and move */
    jonk_update(p, new);
    size = node_size(p, &fstArg, &lstArg, &hasPtrs);
    wSize = wordsof(Node) + size;
    jonk_moveOne(p, wSize, new);
    new += wSize;
    p = (Node*)(((Word*)p) + wSize);
  }
  /* and we're done */
  SHOW(printf("MOVE finished p = %p[%d], hp = %p[%d], new hp = %p[%d]\n", p, HEAPOFFS(p),
       G_hp, HEAPOFFS(G_hp), new, HEAPOFFS(new)));
  G_hp = new;
#if DEBUG
  for (; new < (Word*)G_hpEnd; new++){
    *new = 0xFFFFFFFF; /* make it easier to debug since things die very fast */
  }
#endif
}

/* run a complete heap collection */
void jonk_collect(){
  G_moveCount = 0;


  G_gcStats.wordsUsed += G_hp - G_hpStart;

  /*  sanity_heap(false);*/
  SHOW(printf("COLLECTION STARTED  =============================\n"));

  /* do collection */
  mark();
  jonk_threadCAFs();
  jonk_threadStack();
  jonk_threadGlobals();
  jonk_threadStablePtrs();
  jonk_scan();
  jonk_move();

  /* collect stats */
  G_gcStats.wordsMoved += G_hp - G_hpStart;
  G_gcStats.maxLive = MAX(G_gcStats.maxLive, G_hp - G_hpStart);
  G_gcStats.numGCs++;

#if 0
  if (G_moveCount != G_markCount){
    printf("Marked %d nodes, moved %d\n", G_markCount, G_moveCount);
    abort();
  }
#endif
  /*  printf("%d words live after GC\n", G_hp - G_hpStart);*/
#if DEBUG
  printf("COLLECTION FINISHED *****************************\n");
  SHOW(printf("\t%d words are now free\n", G_hpEnd - G_hp));
#endif
}

