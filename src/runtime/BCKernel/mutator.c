/* the mutator, runs the appropriate bytecodes for a description of what each bytecode does
   see bytecodes.h */
#include "node.h"
#include "heap.h"
#include "pretty.h"
#include "integer.h"
#include "external.h"
#include "primitive.h"
#include "profile.h"
#include "process.h"
#include "hsffi.h"

/* debug traces every instructions */
#define DEBUG 0

/* very paranoid checks the whole heap after each instruction
   mind bogglingly slow, but very good for finding pointer mistakes in the interpretter */
#define VERY_PARANOID 0

/* quite paranoid checks the whole heap after every NEED_HEAP, this is
   still quite slow but quite handy */
#define QUITE_PARANOID 0

/* always need heap - if 1 then it does a complete GC at every needheap
   again, incredibly slow but it really works bugs out of the GC */
#define ALWAYS_NEED_HEAP 0

/* overflow paranoid is very paranoid about the heap overflowing */
#define OVERFLOW_PARANOID 0

#if DEBUG
# define SHOW(x) x
#else
# define SHOW(x)
#endif

/* configure options for trace */
#define SHOW_FRAMES 1
#define SHOW_CONSTS 1
#define SHOW_STACK 1
#define SHOW_EQUATIONS 0
#define SHOW_WITH_CUTOFF 0
#define SHOW_CUTOFF <0

#define ZAP_ARGS 1

#if ZAP_ARGS
# define WHEN_ZAP(x) x
#else
# define WHEN_ZAP(x)
#endif

#define ASSERT(x)  assert(x)

Int64 G_insCount = 0;

//#if DEBUG
void trace(CodePtr ip, Node** sp, Frame* fp, Word* hp, Node* vapptr);
//#endif

/* include important mutator macros */
#include "muthelpers.h"

/* initialize the mutator */
void mut_init(){
  G_proc = proc_alloc();

  G_spBase = (Node**)&G_proc->stack->data[G_proc->stack->size];
  G_spLimit = (Node**)G_proc->stack->data;
  G_sp = G_spBase;
  G_insBeforeSwitch = INS_PER_THREAD;
}

/* evaluate a node */
void run(Node* top){
  CodePtr ip = NULL;
  Node** sp = NULL;
  Frame* fp = NULL;
  Word* hp = NULL;
  Node* nodeptr = NULL;
  Node* vapptr = NULL;
  ConstItem* constptr = NULL;
  Int i,n;

#if USE_GCC_LABELS
# define ins(x) &&L_##x
  static void *labels[] = {
#   include "_bytecodes.h"
  };
# undef ins
#endif

  /* initialize mutator */
  mut_init();

  /* set some variables */
  sp = G_sp;
  hp = G_hp;

  *--sp = top;
  goto I_Eval;

  while (true){
    PROFILE_RECORD(*ip);
    Switch
      {
        /* include the code for each instruction from the mutins header */
#       include "mutins.h"
      }
#if !USE_GCC_LABELS
    default:
      printf("implement me!");
      pr_ins(0, ip);
      printf("\n");
      abort();
#endif

   EndSwitch

  }
}

#if DEBUG
/* trace is used to debug the mutator by printing the current state of the machine */
void trace(CodePtr ip, Node** sp, Frame* fp, Word* hp, Node* vapptr){
  Node** p;
  Frame* f;
  Info* info = NODE_INFO(vapptr);
  FInfo* finfo;
  static Int i = 0;

  REMOVE_TINFO(info);
  finfo = PINFO_FINFO(info);

  ASSERT(finfo->info.tag == I_FINFO);

#if SHOW_WITH_CUTOFF
  if (G_insCount SHOW_CUTOFF){
    return;
  }
#endif

  G_hp = hp;
  G_sp = sp;

#if SHOW_EQUATIONS
  if (*ip != RETURN && *ip != RETURN_EVAL && *ip != SELECT_P1 &&
      *ip != SELECT_P2 && *ip != SELECT_0 && *ip != SELECT_1){
    return;
  }
  pr_node(vapptr,3); printf(" = "); pr_node(*sp,3); printf("\n\n");
  return;
#endif

  /*  if (*ip != EVAL && *ip != RETURN && *ip != RETURN_EVAL){
    return;
    }*/

  printf("%c[33m-%Ld--[Thread%d]--%p:[ ",27, G_insCount,G_proc->id,vapptr);
  pr_node(vapptr,2);
  printf(" ]%c[0m\n",27);
#if SHOW_STACK
  printf("stack [size = %d]:\n", finfo->stack);
  printf("\t================\n");
  for (p = sp; p < (Node**)fp; p++){
    printf("\t");
    pr_node(*p,3);
    printf("\n");
  }
  printf("\t----------------\n");
#if SHOW_FRAMES
  printf("\t%s\n", finfo->name);
  for (f = fp; f; f = f->fp){
    FInfo* finfo;

    if (f->vapptr){
      Info* info = NODE_INFO(f->vapptr);

      REMOVE_TINFO(info);

      finfo = PINFO_FINFO(info);
      ASSERT(finfo->info.tag == I_FINFO);
      printf("\t%s\n", finfo?finfo->name:"NOFINFO!");
    }
  }
#endif
  printf("\t================\n");
#endif
  printf("Exception: %p\n", G_proc->stack->exceptionStack);
#ifdef HAT
  hgm_printStack();
#endif
  printf("INS: [%p] %c[32m ", ip, 27);pr_ins(0, ip);printf("%c[0m\n", 27);
  printf("HP: %p\tLIM: %p\t\tSP: %p\tFP: %p\tLIM: %p\tBASE: %p\n", hp,G_hpEnd,sp,fp,G_spLimit,G_spBase);
#if 0
  if (hp > (Word*)sp){
    printf("\t%c[31;mHP is over the limit!%c[0;m\n",27,27);
  }
  if (sp < (Node**)hp || sp >= G_spBase){
    printf("\t%c[31;mSP is over the limit!%c[0;m\n",27,27);
  }
  if (fp < (Frame*)hp || fp >= (Frame*)G_spBase){
    printf("\t%c[31;mFP is over the limit!%c[0;m\n",27,27);
  }
#endif
#if SHOW_CONSTS
  printf(".....%d Consts:...........................\n", finfo->numConsts);
  if (finfo){
    int j;
    for (j = 0; j < finfo->numConsts; j++){
      printf("%d.\t", j);
      pr_constItem(finfo->constTypes[j], finfo->constTable[j]);
      printf("\n");
    }
  }else{
    printf("\tNO FINFO!\n");
  }
  printf(".......................................\n");
#endif
}
#endif
