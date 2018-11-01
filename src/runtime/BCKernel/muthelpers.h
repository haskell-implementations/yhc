/* useful macros for the mutator */

#ifndef __muthelpers_h__
#define __mulhelpers_h__

/* the type of an element in a lookup switch */
typedef struct _LookupSwElem {
  UInt16      tag;
  UInt16      jump;
}LookupSwElem;

/* the type of an element in an int switch */
typedef struct _IntSwElem {
  Int16       tag;
  UInt16      jump;
}IntSwElem;

/* paranoia macros */
#if VERY_PARANOID
# define WHEN_VPARANOID(x) x
#else
# define WHEN_VPARANOID(x)
#endif

#if OVERFLOW_PARANOID
# define WHEN_OVERFLOW_PARANOID(x) x
#else
# define WHEN_OVERFLOW_PARANOID(x)
#endif

/* macros to access the various arguments */
# define I_ARG_UB *ip
# define I_ARG_US *((UInt16*)ip)
# define I_ARG_B  *((Int8*)ip)
# define I_ARG_S  *((Int16*)ip)
# define I_ARG_UB2 *(ip+1)
# define I_ARG_UB3 *(ip+2)
# define I_ARG_US3 *((UInt16*)(ip+2))

/* NOTE:
    PUSH_FRAME and POP_FRAME are used to save and restore the current frame within
    the interpetter.

    PRE_SWITCH and POST_SWITCH are used when the interpetter is switching out to C code,
    or when it could cause a context switch (for example heap_gc, primitive calls, proc_switch).

    So difference is PRE_SWITCH/POST_SWITCH is happy if the stack gets moved wherease
    PUSH_FRAME/POP_FRAME are not.
*/
/* macros to push and pop frames from the frame stack */
#define PUSH_FRAME() \
  G_fp = fp; \
  sp -= FRAME_SIZE; \
  fp = (Frame*)sp; \
  fp->ip = ip; \
  fp->fp = G_fp; \
  fp->vapptr = vapptr

#define POP_FRAME() \
  ip = fp->ip; \
  vapptr = fp->vapptr; \
  sp = (Node**)fp + FRAME_SIZE; \
  fp = fp->fp; \
  { Info* info = NODE_INFO(vapptr); \
    REMOVE_TINFO(info); \
    constptr = PINFO_FINFO(info)->constTable; \
  }

/* macros for handling before and after context switchs */
#define PRE_SWITCH() \
  PUSH_FRAME(); \
  G_fp = G_proc->saveFP = fp; \
  G_sp = sp; G_hp = hp

#define POST_SWITCH() \
  fp = G_proc->saveFP; \
  hp = G_hp; \
  POP_FRAME(); \
  G_spLimit = (Node**)G_proc->stack->data; \
  G_spBase = (Node**)&G_proc->stack->data[G_proc->stack->size]

/* macro to update result */
#define UPDATE_RESULT(upd, res) \
  if (NODE_INFO(upd)->tag == I_TINFO){ \
    proc_unblockHoled(upd); \
  } \
  MAKE_NODE(upd, res, N_IND)

/* macros to switch between case implementation and gcc computed gotos */
#if USE_GCC_LABELS
# define Case(x) }L_##x:{
# define Break   WHEN_OVERFLOW_PARANOID(assert(hp >= G_hpStart && hp < G_hpEnd)); \
                 SHOW(trace(ip, sp, fp, hp, vapptr)); \
                 WHEN_VPARANOID(PRE_SWITCH(); sanity_heap(false); POST_SWITCH()); \
                 G_insCount++; \
                 ip++; goto *labels[*(ip-1)]
# define Switch  Break;
# define EndSwitch
#else
# define Case(x)  }case x:{
# define Break   WHEN_OVERFLOW_PARANOID(assert(hp >= G_hpStart && hp < G_hpEnd)); \
                 SHOW(trace(ip, sp, fp, hp, vapptr)); \
                 WHEN_VPARANOID(PRE_SWITCH(); sanity_heap(false); POST_SWITCH()); \
                 break
# define Switch  switch(*ip++){
# define EndSwitch }
#endif

/* macros to build certain primitive data types */
#define MAKE_CHAR(node, i)                      \
        node = (INode*)hp;                      \
        MAKE_NODE(node, &G_infoChar, N_NORMAL); \
        INIT_HATNODE(node, NULL);               \
        hp += wordsof(INode);                   \
        node->value = i;

#define MAKE_INT(node, i)                      \
        node = (INode*)hp;                     \
        MAKE_NODE(node, &G_infoInt, N_NORMAL); \
        INIT_HATNODE(node, NULL);              \
        hp += wordsof(INode);                  \
        node->value = i;

#define MAKE_FLOAT(node, i)                      \
        node = (FloatNode*)hp;                   \
        MAKE_NODE(node, &G_infoFloat, N_NORMAL); \
        INIT_HATNODE(node, NULL);                \
        hp += wordsof(FloatNode);                \
        node->value = i;

#define MAKE_DOUBLE(node, i)                      \
        node = (DoubleNode*)hp;                   \
        MAKE_NODE(node, &G_infoDouble, N_NORMAL); \
        INIT_HATNODE(node, NULL);                 \
        hp += wordsof(DoubleNode);                \
        node->value = i;

#define MAKE_STRING(node, i)                      \
        node = (StringNode*)hp;                   \
        MAKE_NODE(node, &G_infoString, N_NORMAL); \
        INIT_HATNODE(node, NULL);                 \
        hp += wordsof(StringNode);                \
        node->string = i;

#define MAKE_CONS(node, left,right)               \
        node = (Node*)hp;                         \
        MAKE_NODE(node, G_infoCons, N_NORMAL);    \
        INIT_HATNODE(node, NULL);                 \
        hp += wordsof(Node) + 2;                  \
        node->args[0] = left;                     \
        node->args[1] = right

#define MAKE_AP1(node, info,arg)                  \
        node = (Node*)hp;                         \
        MAKE_NODE(node, info, N_NORMAL);          \
        INIT_HATNODE(node, NULL);                 \
        hp += wordsof(Node) + 1;                  \
        node->args[0] = arg

/* integer operator macros */
#define BINOP2_W(code, op)                    \
   Case(code)                                 \
        INode* x = (INode*)*sp++;             \
        INode* y = (INode*)*sp++;             \
        INode* res;                           \
        REMOVE_IND(x, INode*);                \
        REMOVE_IND(y, INode*);                \
        MAKE_INT(res, x->value op y->value)   \
        *--sp = (Node*)res;                   \
        Break;

#define CMPOP2_W(code, op)                          \
   Case(code)                                       \
       INode* x = (INode*)*sp++;                    \
       INode* y = (INode*)*sp++;                    \
       REMOVE_IND(x, INode*);                       \
       REMOVE_IND(y, INode*);                       \
       if (x->value op y->value){                   \
         *--sp = G_nodeTrue;                        \
       }else{                                       \
         *--sp = G_nodeFalse;                       \
       }                                            \
       Break

#define BINOP1_W(code, op)                    \
    Case(code)                                \
        INode* x = (INode*)*sp++;             \
        INode* res;                           \
        REMOVE_IND(x, INode*);                \
        MAKE_INT(res, op x->value);           \
        *--sp = (Node*)res;                   \
        Break

/* floating point operator macros */
#define BINOP2_F(code, op)                    \
   Case(code)                                 \
        FloatNode* x = (FloatNode*)*sp++;     \
        FloatNode* y = (FloatNode*)*sp++;     \
        FloatNode* res;                       \
        REMOVE_IND(x, FloatNode*);            \
        REMOVE_IND(y, FloatNode*);            \
        MAKE_FLOAT(res, x->value op y->value) \
        *--sp = (Node*)res;                   \
        Break;

#define CMPOP2_F(code, op)                          \
   Case(code)                                       \
       FloatNode* x = (FloatNode*)*sp++;            \
       FloatNode* y = (FloatNode*)*sp++;            \
       REMOVE_IND(x, FloatNode*);                   \
       REMOVE_IND(y, FloatNode*);                   \
       if (x->value op y->value){                   \
         *--sp = G_nodeTrue;                        \
       }else{                                       \
         *--sp = G_nodeFalse;                       \
       }                                            \
       Break

#define BINOP1_F(code, op)                    \
    Case(code)                                \
        FloatNode* x = (FloatNode*)*sp++;     \
        FloatNode* res;                       \
        REMOVE_IND(x, FloatNode*);            \
        MAKE_FLOAT(res, op x->value);         \
        *--sp = (Node*)res;                   \
        Break

/* double point operator macros */
#define BINOP2_D(code, op)                     \
   Case(code)                                  \
        DoubleNode* x = (DoubleNode*)*sp++;    \
        DoubleNode* y = (DoubleNode*)*sp++;    \
        DoubleNode* res;                       \
        REMOVE_IND(x, DoubleNode*);            \
        REMOVE_IND(y, DoubleNode*);            \
        MAKE_DOUBLE(res, x->value op y->value) \
        *--sp = (Node*)res;                    \
        Break;

#define CMPOP2_D(code, op)                          \
   Case(code)                                       \
       DoubleNode* x = (DoubleNode*)*sp++;          \
       DoubleNode* y = (DoubleNode*)*sp++;          \
       REMOVE_IND(x, DoubleNode*);                  \
       REMOVE_IND(y, DoubleNode*);                  \
       if (x->value op y->value){                   \
         *--sp = G_nodeTrue;                        \
       }else{                                       \
         *--sp = G_nodeFalse;                       \
       }                                            \
       Break

#define BINOP1_D(code, op)                    \
    Case(code)                                \
        DoubleNode* x = (DoubleNode*)*sp++;   \
        DoubleNode* res;                      \
        REMOVE_IND(x, DoubleNode*);           \
        MAKE_DOUBLE(res, op x->value);        \
        *--sp = (Node*)res;                   \
        Break

#endif
