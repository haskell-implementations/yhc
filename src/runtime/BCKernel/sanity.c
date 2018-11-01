#include "heap.h"
#include "mark.h"
#include "integer.h"
#include "pretty.h"
#include "primitive.h"
#include "process.h"
#include "stable.h"

#define DEBUG 0

#if DEBUG
# define SHOW(x) x
#else
# define SHOW(x)
#endif

extern Int64 G_insCount;

Int G_sanityCount = 0;

/* sanity checking, checks things are the way we expect.
   this is mostly just checking that things have stayed the right size across
   different platforms. */
void sanity_init(){
  UInt my = 0x12345678;
  UByte* p = (UByte*)&my;

  /* has the endian flag been defined correctly ?*/
#if IS_BIG_ENDIAN
  UByte lsb = p[sizeof(Word)-1];
#else
  UByte lsb = p[0];
#endif
  if (lsb != 0x78){
    fprintf(stderr, "sanity: expected LSB of 0x12345678 to be 0x78, but got 0x%02x\n", lsb);
    fprintf(stderr, "\tis IS_BIG_ENDIAN defined correctly?\n");
    exit(1);
  }
  /* are things that are supposed to be the same size actually so? */
  if (sizeof(Word) != sizeof(void*)){
    fprintf(stderr, "sanity: sizeof(Word) = %ld, sizeof(void*) = %ld\n", (unsigned long)sizeof(Word), (unsigned long)sizeof(void*));
    exit(1);
  }
  if (sizeof(Int) != sizeof(void*)){
    fprintf(stderr, "sanity: sizeof(Int) = %ld, sizeof(void*) = %ld\n", (unsigned long)sizeof(Int), (unsigned long)sizeof(void*));
    exit(1);
  }
  /* are the word size constants correct? */
  if (sizeof(Word) != WORD_BYTES){
    fprintf(stderr, "sanity: sizeof(Word) = %ld, WORD_BYTES = %d\n", (unsigned long)sizeof(Word), WORD_BYTES);
    exit(1);
  }
  if (sizeof(Word)*BYTE_BITS != WORD_BITS){
    fprintf(stderr, "sanity: sizeof(Word) = %ld, WORD_BITS = %d\n", (unsigned long)sizeof(Word), WORD_BITS);
    exit(1);
  }
  /* do things fit correctly into word sized chunks? */
  if (wordsof(Node)*sizeof(Word) != sizeof(Node)){
    fprintf(stderr, "sanity: wordsof(Node) = %d, sizeof(Node) = %d\n", wordsof(Node), sizeof(Node));
    exit(1);
  }
  if (wordsof(INode)*sizeof(Word) != sizeof(INode)){
    fprintf(stderr, "sanity: wordsof(INode) = %ld, sizeof(INode) = %ld\n", (unsigned long)wordsof(INode), (unsigned long)sizeof(INode));
    exit(1);
  }
  if (wordsof(FloatNode)*sizeof(Word) != sizeof(FloatNode)){
    fprintf(stderr, "sanity: wordsof(FloatNode) = %ld, sizeof(FloatNode) = %ld\n",
            (unsigned long)wordsof(FloatNode), (unsigned long)sizeof(FloatNode));
    exit(1);
  }
  if (wordsof(DoubleNode)*sizeof(Word) != sizeof(DoubleNode)){
    fprintf(stderr, "sanity: wordsof(DoubleNode) = %ld, sizeof(DoubleNode) = %ld\n",
            (unsigned long)wordsof(DoubleNode), (unsigned long)sizeof(DoubleNode));
    exit(1);
  }
  if (wordsof(IntegerNode)*sizeof(Word) != sizeof(IntegerNode)){
    fprintf(stderr, "sanity: wordsof(IntegerNode) = %ld, sizeof(IntegerNode) = %ld\n", (unsigned long)wordsof(IntegerNode),
            (unsigned long)sizeof(IntegerNode));
    exit(1);
  }
}

/****************************************************************************************************/

Node* sanity_nodeHead(Node* p, Info** rinfo);

/* pretty print a node as best as possible */
void sanity_print(Node* p){
  Info* i;
  Node* q;

  printf("{%p[%ld]:", p, (unsigned long)HEAP_OFFS(p));
  for (q = p; NODE_FLAGS(q) == N_IND; q = (Node*)NODE_INFO(p)){
    printf("*");
  }
  i = NODE_INFO(q);
  for (i = NODE_INFO(q); i->tag == I_TINFO; i = (Info*)(((Process*)i)->next)){
    printf("-");
  }
  if (i->tag == I_CINFO){
    CInfo* cinfo = (CInfo*)i;
    printf("C%s(%d)}",cinfo->name,cinfo->size);
  }else if (i->tag == I_PINFO){
    PInfo* pinfo = (PInfo*)i;
    FInfo* finfo = PINFO_FINFO(pinfo);
    Bool isFinfo = finfo->info.tag == I_FINFO || finfo->info.tag == I_XINFO;
    char* name = isFinfo ? finfo->name : "??";
    printf("F%s/%d}", name, pinfo->need);
  }else{
    printf("?? (%p,tag=%d)}", i, i->tag);
  }
}

/* check that indirections reach a sensible place */
Node* sanity_ind(Node* p){
  Node* q;
  if (p < (Node*)G_hpStart || p >= (Node*)G_hp){
    printf("sanity_ind: node %p is outside the heap! (%p[0] <-> %p[%ld])\n", p, G_hpStart, G_hp, (unsigned long)HEAP_OFFS(G_hp));
    return NULL;
  }
  if (NODE_FLAGS(p) != N_IND){
    return p;
  }
  q = (Node*)NODE_INFO(p);
  q = sanity_ind(q);
  if (!q){
    printf("\tfollowed indirection from %p[%ld]\n", p, (unsigned long)HEAP_OFFS(p));
    return NULL;
  }
  return q;
}

/* sanity check a function */
Bool sanity_function(FInfo* finfo){
  Int i;

  if (finfo->info.tag != I_FINFO && finfo->info.tag != I_XINFO){
    printf("sanity_function: finfo is not a valid FInfo/XInfo, tag = %d\n", finfo->info.tag);
    return false;
  }
  if (finfo->info.tag == I_XINFO || finfo->link == finfo){
    return true;
  }
  finfo->link = finfo;
  for (i = 0; i < finfo->numConsts; i++){
    if (finfo->constTypes[i] == C_NODE){
      Node* p = (Node*)finfo->constTable[i];
      if (!sanity_nodeHead(p, NULL)){
        printf("\tfollowed %s const table entry %ld\n", finfo->name, i);
        return false;
      }
    }else if (finfo->constTypes[i] == C_INFO){
      Info* info = (Info*)finfo->constTable[i];
      if (info->tag == I_FINFO || info->tag == I_XINFO){
        if (!sanity_function((FInfo*)info)){
          printf("\tfollowed %s const table entry %ld\n", finfo->name, i);
          return false;
        }
      }
    }
  }
  finfo->link = NULL;
  return true;
}

/* check that a node header is sensible (excluding arguments) */
Node* sanity_nodeHead(Node* p, Info** rinfo){
  Node* q = sanity_ind(p);
  Info* info;
  if (!q){
    return NULL;
  }
  info = NODE_INFO(q);
  REMOVE_TINFO(info);
  if (info->tag == I_PINFO){
    FInfo* finfo = PINFO_FINFO(info);
    if (!sanity_function(finfo)){
      printf("\tfollowed finfo for node ");sanity_print(p);printf("\n");
      return NULL;
    }
  }else if (info->tag != I_CINFO){
    printf("sanity_nodeHead:  the info for node ");sanity_print(p);printf(" is neither CInfo nor PInfo!\n");
    return NULL;
  }
#ifdef HAT
  {
    HNode* hnode = &p->hatNode;
    if (hnode->flags != HNFL_UNTRACED && hnode->flags != HNFL_TRACED && hnode->flags != HNFL_TRACED_FW){
      printf("sanity_nodeHead: the hatnode info for node ");sanity_print(p);printf(" has invalid flags (0x%x)\n", hnode->flags);
      return NULL;
    }
  }
#endif
  if (rinfo){
    *rinfo = info;
  }
  return q;
}

/* check that a node and it's arguments is sensible */
Bool sanity_node(Node* p, Int* size){
  Int i, fstArg, lstArg;
  Bool hasPtrs;
  Node* q;
  Info* info;

  q = sanity_nodeHead(p, &info);
  if (!q){
    return false;
  }
  if (NODE_FLAGS(p) == N_IND){
    /* skip over the rest of the node if it's an indirection,
       otherwise we'll get the size wrong. The linear scan will get the target node
       anyway */
    i = 0;
    /* this is a bit subtle, only a PAP could have become an IND and
       it is guaranteed that the only arguments a PAP has are pointers to
       other heap nodes. All heap pointers are 4 byte aligned so their flags must be
       0 */
    while (NODE_FLAGS((Node*)&p->args[i]) == 0){
      i++;
    }
    *size = i;
    return true;
  }
  /*sanity_print(p);printf("\n");*/
  *size = node_size(q, &fstArg, &lstArg, &hasPtrs);
  for (i = fstArg; i <= lstArg && hasPtrs; i++){
    if (!sanity_nodeHead(q->args[i], NULL)){
      printf("\tfollowed argument %ld of node ",i);sanity_print(p);printf("\n");
      return false;
    }
  }
  return true;
}

/* sanity check a process */
Bool sanity_process(Process* proc){
  Frame* f;
  Int frame = 0;
  /* check the stack node */
  if (!sanity_nodeHead((Node*)proc->stack, NULL)){
    printf("\tfollowed stack node in process %ld\n", proc->id);
    return false;
  }
  if (proc->stack->parent != proc){
    printf("\tproc stack node for process %ld has invalid parent (is %p, should be %p)\n",
      proc->id, proc->stack->parent, proc);
    return false;
  }
  /* check all the frames in the stack */
  for (f = proc->saveFP; f; f = f->fp){
    Node** start;
    Node** p;
    char* fname = f->vapptr ? PINFO_FINFO(NODE_INFO(f->vapptr))->name : "UNNAMED";
    if (f->vapptr){
      if (!sanity_nodeHead(f->vapptr, NULL)){
        printf("\tfollowed vapptr in frame pointer %p('%s',%ld), thread %ld\n", f, fname, frame, proc->id);
        return false;
      }
    }
    start = ((Node**)f) + FRAME_SIZE;
    for (p = start; p < (Node**)(f->fp); p++){
      if (!sanity_nodeHead(*p, NULL)){
        Int offs = p - start;
        printf("\tfollowed stack entry %ld (%p) in function %s (frame %ld), thread %ld\n", offs, p, fname,
               frame, proc->id);
        return false;
      }
    }
    frame++;
  }
  return true;
}

/* sanity check the whole heap */
void sanity_heap(Bool ignored){
  Node* p;
  Process* proc;
  Global* g;
  /* check every heap node */
  p = (Node*)G_hpStart;
  while (p < (Node*)G_hp){
    Int pSize;
    if (!sanity_node(p, &pSize)){
      printf("\twhen linearly scanning the heap %p[%ld]\n", p, (unsigned long)HEAP_OFFS(p));
      abort();
    }
    p = (Node*)((Word*)p + wordsof(Node) + pSize);
  }
  /* check every process */
  for (proc = G_procList; proc; proc = proc->linkNext){
    if (!sanity_process(proc)){
      printf("\twhen sanity checking process %ld\n", proc->id);
      abort();
    }
  }
  /* check every global */
  for (g = G_heapGlobals; g; g = g->next){
    if (!sanity_nodeHead(*g->global, NULL)){
      printf("\twhen scanning global variable %p\n", g->global);
      abort();
    }
  }
}

#if 0
/****************************************************************************************************/

Bool sanity_node(Node* p);

/* check that a CAF is sane, this means checking that it really is a CAF and
   checking that the consttable entries are sensible */
Bool sanity_CAF(FInfo* finfo){
  Int i;

  SHOW(printf("\tsanity_caf %s\n", finfo->name));

  if (finfo->info.tag == I_XINFO){
    return true;
  }
  if (finfo->link == finfo){
    return true;
  }
  finfo->link = finfo;
  for (i = 0; i < finfo->numConsts; i++){
    if (finfo->constTypes[i] == C_NODE){
      Node* p = (Node*)finfo->constTable[i];
      if (!sanity_node(p)){
        printf("\tfollowed %s const table entry %ld\n", finfo->name, i);
        return false;
      }
    }else if (finfo->constTypes[i] == C_INFO){
      Info* info = (Info*)finfo->constTable[i];
      if (info->tag == I_FINFO){
        if (!sanity_CAF((FInfo*)info)){
          printf("\tfollowed %s const table entry %ld\n", finfo->name, i);
          return false;
        }
      }
    }
  }
  finfo->link = NULL;
  return true;
}

/* test whether something is inside the heap */
Bool sanity_isHeap(Node* p){
  return p >= (Node*)G_hpStart && p < (Node*)G_hp;
}

/* follow an indirection */
Bool sanity_ind(Node* p){
  Node* q;
  if (!sanity_isHeap(p)){
    printf("sanity_node %p is outside the heap\n", p);
    printf("tracing back:\n");
    return false;
  }
  if (NODE_FLAGS(p) != N_IND){
    return true;
  }
  q = (Node*)NODE_INFO(p);
  if (!sanity_ind(q)){
    printf("\tfollowed indirection %p\n", p);
    return false;
  }
  return true;
}

/* sanity check a node, check that it is inside the heap, that it has a sensible info and that
   its arguments are also sensible. Returns true if it's okay, false otherwise */
Bool sanity_node(Node* p){
  Info* info;
  Int size, i;
  Bool hasPtrs;
  Int fstArg, lstArg;

  SHOW(printf("sanity_node %p ",p);pr_node(p,2);printf("\n"));

  if (!sanity_ind(p)){
    return false;
  }
  REMOVE_IND(p, Node*);

  while(true) {

    if (NODE_FLAGS(p) != N_IND){
      break;
    }
    p = (Node*)NODE_INFO(p);
  }
  info = NODE_INFO(p);
  REMOVE_TINFO(info);

  if (mkt_mark(p)){
    return true;
  }
  G_sanityCount++;
  size = node_size(p, &fstArg, &lstArg, &hasPtrs);

  if (!hasPtrs){
    return true;
  }
  if (info->tag == I_PINFO){
    size = ((PInfo*)info)->size;
    if (!sanity_CAF(PINFO_FINFO(info))){
      printf("\tfollowed info ptr of node %p\n", p);
      return false;
    }
  }
  for (i = fstArg; i <= lstArg; i++){
    if (!sanity_node(p->args[i])){
      printf("\tfollowed argument %ld of %p ", i, p); pr_node(p,1);printf("\n");
      return false;
    }
  }
  return true;
}

/* sanity check a single process */
void sanity_process(Process* proc){
  Frame* f;
  Int frame = 0;
  /* check the stack node */
  if (!sanity_node((Node*)proc->stack)){
    printf("\tfollowed stack node in process %ld\n", proc->id);
    abort();
  }
  if (proc->stack->parent != proc){
    printf("\tproc stack node for process %ld has invalid parent (is %p, should be %p)\n",
      proc->id, proc->stack->parent, proc);
    abort();
  }
  /* check all the frames in the stack */
  for (f = proc->saveFP; f; f = f->fp){
    Node** start;
    Node** p;
    if (f->vapptr){
      if (!sanity_node(f->vapptr)){
        printf("\tfollowed vapptr in frame pointer %p, thread %ld\n", f, proc->id);
        printf("\t\tins = %Ld\n", G_insCount);
        abort();
      }
    }
    start = ((Node**)f) + FRAME_SIZE;
    for (p = start; p < (Node**)(f->fp); p++){
      if (!sanity_node(*p)){
        char* name = f->vapptr ? PINFO_FINFO(NODE_INFO(f->vapptr))->name : "UNNAMED";
        Int offs = p - start;
        printf("\tfollowed stack entry %ld (%p) in function %s (frame %ld), thread %ld\n", offs, p, name,
               frame, proc->id);
        abort();
      }
    }
    frame++;
  }
}

/* sanity check the whole heap by scanning the stack, also acts as a good
   way to check that the stack is consistent (since it'll die quickly otherwise) */
void sanity_heap(Bool markCheck){
  Global* g;
  Process* proc;
  StablePtr* sp;

  G_sanityCount = 0;

  if (G_hp > (Word*)G_hpEnd){
    printf("\thp (%p) is outside the heap limit (%p)\n", G_hp, G_sp);
    printf("\t\tins = %Ld\n", G_insCount);
    abort();
  }

  if (G_sp < (Node**)G_spLimit || G_sp > G_spBase){
    printf("\tsp (%p) is outside the stack limit [%p -> %p]\n", G_sp, G_spLimit, G_spBase);
    printf("\t\tins = %Ld\n", G_insCount);
    abort();
  }
  if (G_fp < (Frame*)G_spLimit || G_fp > (Frame*)G_spBase){
    printf("\tfp (%p) is outside the stack limit [%p -> %p]\n", G_fp, G_spLimit, G_spBase);
    printf("\t\tins = %Ld\n", G_insCount);
    abort();
  }

  mkt_clear();
  for (proc = G_procList; proc; proc = proc->linkNext){
    sanity_process(proc);
  }

  for (g = G_heapGlobals; g; g = g->next){
    if (!sanity_node(*g->global)){
      printf("\tfollowed global variable %p\n", g->global);
      printf("\t\tins = %Ld\n", G_insCount);
      abort();
    }
  }

  for (sp = G_stablePtrs; sp; sp = sp->next){
    if (!sanity_node(sp->node)){
      printf("\tfollowed stable pointer %p\n", sp);
      printf("\t\tins = %Ld\n", G_insCount);
      abort();
    }
  }

#if 0
  if (markCheck && G_markCount != G_sanityCount){
    printf("Marked %d heap nodes, sanity checked %d\n", G_markCount, G_sanityCount);
    printf("\t\tins = %Ld\n", G_insCount);
    abort();
  }
#endif
}

#endif
/****************************************************************************************************/
