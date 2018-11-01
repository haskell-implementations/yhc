/* a very simple recursive stop-copy collector, not very efficient but very easy to write */

#include "heap.h"
#include "pretty.h"
#include "primitive.h"

#define DEBUG 1

#if DEBUG
# define SHOW(x) x
#else
# define SHOW(x)
#endif

#define ASSERT(x) assert(x)

#if 0

static Word* G_newHp = NULL;

static Word   G_cafTableEnd = 0;
static FInfo* G_cafTable = NULL;

Node* sc_node(Node* p);

/* scavenge a common applicative form */
void sc_caf(FInfo* finfo){
  int i;

  if (finfo->info.tag == I_XINFO){
    /* no const table */
    return;
  }
  ASSERT(finfo->info.tag == I_FINFO);
  if (finfo->link){
    /* we must have already done this one */
    return;
  }
  SHOW(printf("sc_caf %s\n", finfo->name));
  finfo->link = G_cafTable;
  G_cafTable = finfo;

  for (i = 0; i < finfo->numConsts; i++){
    if (finfo->constTypes[i] == C_NODE){
      finfo->constTable[i] = (ConstItem)sc_node((Node*)finfo->constTable[i]);
    }else if (finfo->constTypes[i] == C_INFO){
      Info* info = (Info*)finfo->constTable[i];
      if (info->tag != I_CINFO){
        sc_caf((FInfo*)info);
      }
    }
  }
}

/* scavenge a node from the heap, does the bulk of the work */
Node* sc_node(Node* p){
  Info* info;  
  Node* ret;
  int i, size;
  Bool recurse = true;

  SHOW(printf("sc_node(%p)\n",p));
  ASSERT(p >= (Node*)G_hpStart && p < (Node*)G_hp);
  
  REMOVE_IND(p, Node*);

  if (NODE_FLAGS(p) == N_GC){
    /* already scavenged */
    ret = (Node*)NODE_INFO(p);
    return ret;
  }
  info = NODE_INFO(p);

  if (info->tag == I_CINFO){
    /* special cases ... */
    CInfo* cinfo = (CInfo*)info;
    if (cinfo->flags & CI_NO_PTRS){
      recurse = false;
    }
    size = cinfo->size;
  }else if (info->tag == I_PINFO){
    FInfo* finfo = PINFO_FINFO(info);
    size = ((PInfo*)info)->size;
    sc_caf(finfo);
    /* because we might have already got it */
    if (NODE_FLAGS(p) == N_GC){
      ret = (Node*)NODE_INFO(p);
      return ret;
    }
  }else{
    fprintf(stderr, "sc_node(%p) info is neither CINFO nor PINFO! tag was %d\n", p, info->tag);
    abort();
  }
  ret = (Node*)G_newHp;
  SHOW(printf("moved %p ", p);pr_node(p,1);
       printf(" + %d args %sto %p\n", size, recurse ? "" : "[no-recurse] ", ret));
  G_newHp += wordsof(Node) + size;
  ret->_hidden = p->_hidden;
  MAKE_NODE(p, ret, N_GC);

  for (i = 0; i < size; i++){
    if (recurse){
      ret->args[i] = sc_node(p->args[i]);
    }else{
      ret->args[i] = p->args[i];
    }
  }  
  return ret;
}

void sc_collect(){
  Node** src;
  Node** dst;
  Frame* f;
  Frame* g;
  Node** start;
  Node** newSpBase;
  Word* newHpStart;
  Word* newHpEnd;
  FInfo* finfo;
  FInfo* next;
  Global* gp;
  
  // allocate the new heap
  newHpStart = (Word*)malloc(G_hpSize * sizeof(Word));
  newHpEnd = newHpStart + G_hpSize;
  G_newHp = newHpStart;

  // find the position for the new stack
  newSpBase = (Node**)newHpEnd;

  // start the caf linking
  G_cafTable = (FInfo*)&G_cafTableEnd;

  // now scan the old stack and update to the new stack
  start = G_sp;
  for (f = G_fp; f < (Frame*)G_spBase; f = f->fp){
    // copy and scavenge the stack entries
    dst = newSpBase - (G_spBase - start);
    for (src = start; src < (Node**)f; src++,dst++){
      *dst = sc_node(*src);
    }
    start = ((Node**)f) + FRAME_SIZE;
    // then copy the stack frame
    g = (Frame*)(newSpBase - (G_spBase - (Node**)f));
    *g = *f;
    // then scavenge the vapptr
    g->vapptr = f->vapptr ? sc_node(f->vapptr) : NULL;
    // and update the frame pointer
    g->fp = (Frame*)(newSpBase - (G_spBase - (Node**)f->fp));
  }
  // deal with globals
  for (gp = G_primGlobals; gp; gp = gp->next){
    *gp->global = sc_node(*gp->global);
  }
  // flip the heap
  memset(G_hpStart, 0xFF, G_hpSize * sizeof(Word));
  free(G_hpStart);
  G_hpStart = newHpStart;
  G_hpEnd   = newHpEnd;
  G_hp      = G_newHp;
  
  // now update the other pointers
  G_sp     = newSpBase - (G_spBase - G_sp);
  G_fp     = (Frame*)(newSpBase - (G_spBase - (Node**)G_fp));
  G_spBase = newSpBase;

  // finally clear all the CAf link fields
  for (finfo = G_cafTable; finfo != (FInfo*)&G_cafTableEnd; finfo = next){
    next = finfo->link;
    finfo->link = NULL;    
  }

  SHOW(printf("END COLLECTION!*********************************************************\n"));
}



#endif
