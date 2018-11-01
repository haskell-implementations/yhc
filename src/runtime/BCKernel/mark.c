/*   -----------------------------------------------------------------------------------------------------------
     How it works
     -----------------------------------------------------------------------------------------------------------

     the new pointer reversal marking algorithm, essentially it works like this:

     The stack is formed by chaining together pointers using pointer reversal.
     So to walk this stack we might use:

     for (p = last; p != NULL; p = (Node**)*p)

     i.e. the contents of each cell is the next link in the chain.

     When a node being marked needs to mark its arguments it sets the last
     argument field to be the previous 'last' stack link. It then sets last to
     the argument field updated and sets then jumps to evaluate the argument.

     It also marks the first argument in the mark table. Thus the node looks like

     +######+-----------+######+------+...+---------+
     |#info#|   extra   |#arg0#| arg1 |   | old-lst |
     +######+-----------+######+------+...+---------+
                                             ^
                                      last __|

     With #s showing the marked fields. Now when we want to 'return' we
     look at the cell under the 'last' pointer. If the 'last' pointer is null then we've
     reached the end of the recursion so return from the mark function.

     If it's not null and the cell is marked then we've just returned from the final argument:
     restore the argument pointer, 'pop' this item from the stack and then jump to the
     code to return again (which will either mark more arguments or will itself return).

     If it's not marked then there are arguments left: decrement the last pointer, restore the previous
     argument and jump to the code to mark the new argument.

     The other thing that the code does is thread in new CAFs as it marks. Then as the last stage
     of the mark algorithm it scans all the CAFs, marking any nodes that are in the constant table
     and adding any new CAFs to the *end* of the CAF list (if it added them to the beginning it would miss them).

     -----------------------------------------------------------------------------------------------------------
     Arrays
     -----------------------------------------------------------------------------------------------------------

     Arrays cause a special complication in this by being as follows

     +######+-----------+######+------+...+---------+
     |#info#|   extra   |#size#| elm0 |   | old-lst |
     +######+-----------+######+------+...+---------+
                                              ^
                                       last___|

     The size part needs to be marked because it needs to be the first word after the info+extra. However,
     we have to be very careful not to accidentally recurse down the size field!

     -----------------------------------------------------------------------------------------------------------
     ForiegnObj
     -----------------------------------------------------------------------------------------------------------

     ForeignObjs cause a minor problem

     +######+-----------+######+------+
     |#info#|   extra   |#final| ptr  |
     +######+-----------+######+------+
                           ^
                    last___|

     The final field should be marked, the ptr field should not. This is solved by simply starting at the 'final' entry.

*/

#include "heap.h"
#include "pretty.h"
#include "primitive.h"
#include "foreign.h"
#include "process.h"
#include "stable.h"
#include "mark.h"
#include "muthelpers.h"

#define DEBUG 0

#if DEBUG
# define SHOW(x) x
#else
# define SHOW(x)
#endif

#define ASSERT(x) assert(x)

Int    G_markCount = 0;
FInfo* G_lastCaf = NULL;
FInfo* G_firstCaf = NULL;

/*---------------------------------------------------------------------------------------------------------*/

#define GET_MARK_OFFS(p,w,x)  \
  { Word _diff = (Word*)p - (Word*)G_hpStart; \
    w = _diff >> WORD_BITS_SHIFT; \
    x = ((Word)1) << (_diff & WORD_BITS_MASK); }

/* clear the whole mark table */
void mkt_clear(){
  memset(G_markTable, 0, G_markTableSize * sizeof(Word));
}

/* test whether the pointer is marked */
Bool mkt_isMarked(Node* p){
  Word w, x;

  if (!(p >= (Node*)G_hpStart && p < (Node*)G_hp)){
    ASSERT(p >= (Node*)G_hpStart && p < (Node*)G_hp);
  }

  GET_MARK_OFFS(p,w,x);

  ASSERT(w >= 0 && w < G_markTableSize);

  return (G_markTable[w] & x);
}

/* mark the specified pointer and return whether it was marked previously */
Bool mkt_mark(Node* p){
  Word w, x;
  Bool ret;

  ASSERT(p >= (Node*)G_hpStart && p < (Node*)G_hp);

  GET_MARK_OFFS(p,w,x);

  ASSERT(w >= 0 && w < G_markTableSize);

  ret = G_markTable[w] & x;
  G_markTable[w] |= x;
  return ret;
}

/* unmark the specified pointer and return whether it was marked previous */
Bool mkt_unmark(Node* p){
    Word w, x;
    Bool ret;

    ASSERT(p >= (Node*)G_hpStart && p < (Node*)G_hp);
    GET_MARK_OFFS(p,w,x);
    ASSERT(w >= 0 && w < G_markTableSize);
    ret = G_markTable[w] & x;
    G_markTable[w] &= ~x;
    return ret;
}

/* this is some clever trickery to quickly find the first marked item in the
   marked table after the given node. It relies on fast integer arithmetic to
   check it 32 items at a time. It's a little fiddly, but because the mark table
   is typically 95% empty it can give a 20% speedup on GC heavy programs compared
   to doing it naively */
Node* mkt_firstMarked(Node* node){
  Word w,x,chk;
  Word* p = (Word*)node;
  Word* base;

  GET_MARK_OFFS(p,w,x);

  /* work out what word in the heap 'w' is refering to, this is the base pointer */
  base = G_hpStart + (w << WORD_BITS_SHIFT);

  /* get the mark table entry at w and clear all the bits before x. i.e. ignore
     pointers before 'node'.*/
  chk = G_markTable[w];
  chk &= ~(x - 1);

  /* if the first word is empty keep trying new ones */
  if (!chk){
    x = 1;  /* the word is empty so check from the beginning */
    /* now check the whole word in one go */
    do {
      /* whole word blank, so move onto the next one */
      w++;                     /* next word */
      base += WORD_BITS;       /* next WORD_BITS (e.g. 32) pointers */
      if (base >= (Word*)G_hp){
        /* stop if we've gone over the end of the heap */
        return (Node*)base;
      }
      chk = G_markTable[w];    /* get that word */
    }while (!chk);
    p = base;
  }
  /* scan through from x */
  while (true){
    if (chk & x){
      return (Node*)p;
    }
    p++;
    x <<= 1;
  }
}

/*---------------------------------------------------------------------------------------------------------*/

/* print the current mark stack, really just used for debugging */
#if DEBUG
static void pr_mark_state(Node* p, Node** last){
  pr_node(p, 1);
#if 0
  printf(" : [");

  while(last){
    Node** q = last;
    Node* x;
    Int i = 0;

    while (!mkt_isMarked((Node*)q)){
      q--;i++;
    }
    x = (Node*)(q - wordsof(Node));
    printf("%ld:",i);
    pr_node(x,1);
    printf(" ");
    last = (Node**)*last;
  }
  printf("]\n");
#endif
}

static void pr_indent(int in){
  int i;
  printf("%3d ", in);
  for (i = 0; i < in; i++){
    printf("  ");
  }
}
#endif

/* link a caf onto the end of the CAF list, we add onto the end because then
   naively looping down the list of CAFs in mark_CAFs works cleanly. */
static void mark_addCAF(FInfo* finfo){
  if (finfo->link || G_lastCaf == finfo || finfo->info.tag != I_FINFO){
    return;
  }
  if (G_lastCaf){
    G_lastCaf->link = finfo;
  }else{
    G_firstCaf = finfo;
  }
  G_lastCaf = finfo;
}

/* perform wadler's optimisation on a node */
static Node* mark_wadler(Node* p){
    Info* info;
    FInfo* finfo;
    UInt argNum;
    Node* arg;
    Node* fstArg;
    UInt ins;

    /* is the first argument a constructor? */
    fstArg = p->args[0];
    REMOVE_IND(fstArg, Node*);
    info = NODE_INFO(fstArg);
    if (info->tag != I_CINFO){
        /* then it's not a selector applied to a constructor, so don't bother trying */
        return p;
    }

    /* otherwise work out which arg of the constructor we want */
    info = NODE_INFO(p);
    REMOVE_TINFO(info);
    finfo = PINFO_FINFO(info);
    ins = finfo->code[SELECTOR_SELECT];
    switch (ins){
    case SELECT_0: argNum = 0; break;
    case SELECT_1: argNum = 1; break;
    case SELECT_P1: argNum = finfo->code[SELECTOR_ARG]; break;
    case SELECT_P2: argNum = *(UInt16*)&finfo->code[SELECTOR_ARG]; break;
    default:
        abort();
    }
    arg    = fstArg->args[argNum];
    REMOVE_IND(arg, Node*);

    /* update the original node with the arg, this replaces the selector with an IND to the result.
       That way future attempts to mark it will just skip over the IND straight to the selected item. */
    UPDATE_RESULT(p, arg);

    /* unfortunately p has already been marked, but now it's an IND. Marked INDs aren't allowed, so we need to unmark it */
    mkt_unmark(p);
    return arg;         /* return what was selected */
}

/* mark a node recursively (where recursively is actually iteratively!). Returns the original node updated. */
static Node* mark_rec(Node* p){
  Node** last = NULL;
#ifdef DEBUG
  int indent = 0;
#endif

 mark_node:{
    Int size;
    Int fstArg, lstArg;
    Bool hasPtrs;
    Info* info;

    ASSERT(NODE_FLAGS(p) != N_IND);

    SHOW(pr_indent(indent++); printf("mark_node(%p) ",p);pr_mark_state(p,last));
#if DEBUG
    ASSERT(p >= (Node*)G_hpStart && p < (Node*)G_hp);
#endif

    if (mkt_mark(p)){
      SHOW(printf("\talready marked\n"));
      goto mark_return;
    }
    /* get size */
    size = node_size(p, &fstArg, &lstArg, &hasPtrs);

    /* add the CAF if it has one */
    info = NODE_INFO(p);
    REMOVE_TINFO(info);
    if (info->tag == I_PINFO){
      FInfo* finfo = PINFO_FINFO(info);
      mark_addCAF(finfo);

      /* if this is a selector then wadler it */
      if (size >= 1 && (finfo->flags & FFL_SELECTOR) != 0){
        Node* q = mark_wadler(p);
        /* if we get same node back then we can't short-cut the selector at all */
        if (q != p){
          /* the selector has been cut-out, so now mark the argument */
          p = q;
          goto mark_node;
        }
      }
    }

    /* if this is a ForeignPtr then mark inside */
    if (info->tag == I_CINFO){
      CInfo* cinfo = (CInfo*)info;
      if (cinfo->flags & CI_FOREIGN_PTR){
        BoxNode* box = (BoxNode*)p;
        FOREIGN_MARK((ForeignPtr*)box->ptr);
      }
    }

    /* either return or mark the arguments */
    SHOW(printf("\tsize = %ld, fstArg = %ld, lstArg = %ld\n", size, fstArg, lstArg));
    if (size <= fstArg || !hasPtrs){
      goto mark_return;
    }else{
      Int i = lstArg;
      Node* tmp = p->args[i];

      REMOVE_IND(tmp, Node*);

      p->args[i] = (Node*)last;
      last = &p->args[i];
      mkt_mark((Node*)p->args);
      p = tmp;
      goto mark_node;
    }
  }
 mark_return:{
    SHOW(pr_indent(--indent); printf("mark_return(%p) ",p);pr_mark_state(p,last));
    if (!last){
      SHOW(printf("\tall done!\n"));
      return p;
    }
    if (mkt_isMarked((Node*)last)){
      /* pop node */
      Node** newLast;

      SHOW(printf("\tpopping node\n"));
      newLast = (Node**)*last;
      *last = p;
      p = (Node*)(last - wordsof(Node));
      last = newLast;
      goto mark_return;
    }else{
      /* next arg */
      Node* tmp = *last;

      SHOW(printf("\tnext argument\n"));
      *last = p;
      last--;
      /* array check */
      if (mkt_isMarked((Node*)last)){
        Node* arr = (Node*)(last - wordsof(Node));
        Info* info = NODE_INFO(arr);
        /* NOTE: in this instance we don't to chase info, no I_TINFO will ever end up at a CINFO */
        if (info->tag == I_CINFO && (((CInfo*)info)->flags & CI_ARRAY)){
          /* then this is an array, don't descend into the size! */
          SHOW(printf("\tpopping array\n"));
          p = (Node*)(last - wordsof(Node));
          last = (Node**)tmp;
          goto mark_return;
        }
      }
      p = *last;
      *last = tmp;
      REMOVE_IND(p,Node*);
      goto mark_node;
    }
  }
}

/* Scan the CAFs in the caf list, mark and nodes found the constant table
   and link in any additional CAFs */
static void mark_CAFs(){
  FInfo* finfo;

  /* go through our list of CAFs, note this may add new CAFs but they get added to the end so it works fine */
  for (finfo = G_firstCaf; finfo != NULL; finfo = finfo->link){
    Int i;

    SHOW(printf("marking CAF '%s'\n", finfo->name));

    /* scan through the constants in the CAF */
    for (i = 0; i < finfo->numConsts; i++){
      if (finfo->constTypes[i] == C_NODE){
        Node* p = (Node*)finfo->constTable[i];

        REMOVE_IND(p, Node*);
        finfo->constTable[i] = (ConstItem)mark_rec(p);
      }else if (finfo->constTypes[i] == C_INFO){
        Info* info = (Info*)finfo->constTable[i];
        if (info->tag == I_FINFO){
          mark_addCAF((FInfo*)info);
        }
      }
    }
  }
}

/* main mark routine */
void mark(){
  Node** p;
  Frame* f;
  Node** start;
  Global* g;
  Process* proc;
  StablePtr* sp;

  G_markCount = 0;
  SHOW(printf("----------------------- Mark STARTED ----------------------------\n"));

  /* initialize our CAF list */
  G_firstCaf = G_lastCaf = NULL;

  /* calling memset over the marktable is probably quicker that trying to
     arrange to clear it as we go in the collector ... */
  mkt_clear();

  /* for every process running */
  for (proc = G_procList; proc; proc = proc->linkNext){
    /* scan every frame in the stack */
    for (f = proc->saveFP; f; f = f->fp){
      /* mark the frame vapptr if present */
      if (f->vapptr){
        f->vapptr = mark_rec(f->vapptr);
      }

      /* scan every stack item in the frame */
      start = ((Node**)f) + FRAME_SIZE;
      for (p = start; p < (Node**)f->fp; p++){
        Node* x = *p;

        /* remove any indirections */
        REMOVE_IND(x, Node*);
        /* and mark recursively */
        *p = mark_rec(x);
      }
    }
    /* also mark the stack node */
    proc->stack = (ProcStackNode*)mark_rec((Node*)proc->stack);
    /* and the blockedOn */
    proc->blockedOn = mark_rec(proc->blockedOn);
  }
  SHOW(printf("Marking globals\n"));
  /* now mark the prim globals */
  for (g = G_heapGlobals; g; g = g->next){
    Node* p = *g->global;

    SHOW(printf("Marking global (g=%p, g->global=%p)\n", g, p));
    REMOVE_IND(p, Node*);
    *g->global = mark_rec(p);
  }
  /* mark stable pointers */
  for (sp = G_stablePtrs; sp; sp = sp->next){
    Node* p = sp->node;
    REMOVE_IND(p, Node*);
    sp->node = mark_rec(p);
  }
  /* now mark the CAFs */
  SHOW(printf("Marking CAFs\n"));
  mark_CAFs();
  SHOW(printf("Mark FINISHED\n"));
}
