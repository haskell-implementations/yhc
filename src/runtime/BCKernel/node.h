#ifndef __node_h__
#define __node_h__

#include "types.h"

# include "hat/hat.h"
#ifdef HAT
# define EXTRA wordsof(HNode)
#else
# define EXTRA 0
#endif

/*--------------------------------------------------------------------------------*/

/* typedef enum { N_IND = 0x00, N_NORMAL = 0x01, N_UNMARK = 0x00, N_MARK = 0x02, N_MASK = 0x03 } NodeFlags; */

/* the node flags of a node encode what state the node is in. In general nodes start as normal,
   become holes when evaluated and then are updated to inds. Gc is used to mark indirections in the
   garbage collector */
typedef enum { N_IND = 0x00, N_NORMAL = 0x01, N_GC = 0x02, N_HOLE = 0x03, N_MASK = 0x03  } NodeFlags;

typedef struct _Info Info;
typedef UInt8* CodePtr;

typedef struct _NodeHeader {
  Word             _hidden;
#ifdef HAT
  HNode            hatNode;
#endif
}NodeHeader;

/* A basic heap node format. _hidden is the combined info pointer and flags,
   and is designed to be accessed via the macros only. args acts as apointer to further
   arguments allocated after the node header. */
typedef struct _Node {
  Word             _hidden;

#ifdef HAT
  HNode            hatNode;
#endif

  struct _Node*    args[0];
}Node;


/* integer node */
typedef struct _INode {
  NodeHeader      node;
  Int             value;
}INode;

/* long integer node */
typedef struct _LongNode {
  NodeHeader      node;
  Int64           value;
}LongNode;

/* floating point node */
typedef struct _FloatNode {
  NodeHeader      node;
  Float           value;
}FloatNode;

/* double precision floating point node */
typedef struct _DoubleNode {
  NodeHeader       node;
  Double          value;
}DoubleNode;

/* file handle node */
typedef struct _HandleNode {
  NodeHeader      node;
  FILE*           handle;
}HandleNode;

/* node with a single argument */
typedef struct _Node1 {
  NodeHeader     node;
  Node*          arg0;
}Node1;

/* node pointing to a string */
typedef struct _StringNode {
  NodeHeader     node;
  Char*          string;
}StringNode;

/* an array of objects */
typedef struct _ArrayNode {
  NodeHeader     node;
  UInt           size;
  Word           data[0];
}ArrayNode;

/* a packed string: must be basically the same as 'ArrayNode' */
typedef struct _PackedStringNode {
  NodeHeader     node;
  UInt           size;
  Char           string[0];
}PackedStringNode;

/* box for FFI objects */
typedef struct _BoxNode {
  NodeHeader     node;
  void*          ptr;
}BoxNode;

/* a foreign object */
typedef struct _ForeignPtrNode {
  NodeHeader                node;
  void*                     ptr;
}ForeignPtrNode;

/* an exception handler */
typedef struct _ExceptionHandlerNode {
  NodeHeader                        node;
  struct _ExceptionHandlerNode*     next;           /* next exception handler in the stack */
  Node*                             vapptr;         /* vapptr of the handler code */
  CodePtr                           ip;             /* ip to jump to for the handler code */
  UInt                              spOffs;         /* offset of sp from G_spBase, offset is easier than ptr here because of GC */
  UInt                              fpOffs;         /* offset of fp from G_spBase, again offsets easier */
}ExceptionHandlerNode;

/* node handling macros, generally to handle the combined info & flags field */
#define MAKE_NODE(n, i,f)  ((Node*)(n))->_hidden = ((Word)(i) & ~N_MASK) | ((Word)(f) & N_MASK);
#define NODE_INFO(n)       ((Info*)(((Word)((Node*)(n))->_hidden) & ~N_MASK))
#define NODE_FLAGS(n)      (((Word)(n)->_hidden) & N_MASK)

/*
#define NODE_MARK(n)       ((Node*)(n))->_hidden |= N_MARK
#define NODE_UNMARK(n)     ((Node*)(n))->_hidden &= ~N_MARK
#define NODE_ISMARKED(n)   (((Node*)(n))->_hidden & N_MARK)
*/

#define NODE_SET_FLAGS(n, f) ((Node*)(n))->_hidden = (((Node*)(n))->_hidden & ~N_MASK) | (f)

#define REMOVE_IND(x,t) while (NODE_FLAGS((Node*)x) == N_IND){ x = (t)*(Node**)x; }

/* macro to remove tinfo links */
#define REMOVE_TINFO(info) while((info)->tag == I_TINFO){ (info) = (Info*)(((Process*)(info))->next); }


/*--------------------------------------------------------------------------------*/


/* static information types
      FInfo - information about a function object
      PInfo - information about a partial application to a function, these
              always preceeed the corresponding finfo
      CInfo - information about a constructor
      XInfo - information about an 'external' which is a primitive or foreign call
      TInfo - information about a thread, see process.h */
typedef enum { I_FINFO, I_PINFO, I_CINFO, I_XINFO, I_TINFO } InfoTag;

/* basic field shared by all information structures */
struct _Info {
  HUInt           tag;
};

/* Partial application information, records how many arguments we have and how many remain
   to be satured. */
typedef struct _PInfo {
  Info            info;
  QUInt           size;
  QUInt           need;
}PInfo;

/* for a given PInfo find the corresponding FInfo */
#define PINFO_FINFO(pi)    ((FInfo*)(((PInfo*)pi)+((PInfo*)pi)->need+1))

/* constant table entry, only NODE and INFO are used */
enum ConstType { C_SPACE, C_INFO, C_NODE, C_INT, C_FLOAT, C_DOUBLE, C_STRING, C_POS, C_VAR_DESC };

typedef Word ConstItem;
struct _Module;

enum FInfoFlags { FFL_NONE = 0x00, FFL_INVIS = 0x01, FFL_LAMBDA = 0x02, FFL_PRIM_APPLY = 0x04, FFL_SELECTOR = 0x80 };

/* function info,  must always be preceeded by the pap table for the function of the
   appropriate size. */
typedef struct _FInfo {
  Info             info;
  PInfo*           papTable;      /* partial application table */
  struct _FInfo*   link;          /* for garbage collecting CAFs */
  HUInt            arity;         /* function arity */
  HUInt            stack;         /* function stack usage - UNUSED */
  HUInt            flags;         /* function flags */
  struct _Module*  module;        /* the module this finfo was loaded from */
  Char*            name;          /* function name */

#ifdef HAT
  HNode            hatNode;
  HInfo            hatInfo;
#endif

  /* FInfo specific */
  Int              codeSize;
  CodePtr          code;          /* pointer to byte code */
  HUInt            numConsts;     /* number of constants */
  UByte*           constTypes;    /* type of each constant */
  ConstItem*       constTable;    /* the constants themselves */
}FInfo;

/* a CFunction is an external function in C */
struct _FFIFunction;

/* External function info the basic layout which is in common with FInfo *MUST* be the same
   FIXME: are XInfo's even used any more? */
typedef struct _XInfo {
  Info             info;
  PInfo*           papTable;
  Info*            link;
  HUInt            arity;
  HUInt            stack;
  struct _Module*  module;
  Char*            name;

#ifdef HAT
  HNode            hatNode;
  HInfo            hatInfo;
#endif

  /* XInfo specific */
  struct _FFIFunction*    ffiFunc;
}XInfo;

/* constructor flags, NO_PTRS means that the contents should
   not be treated as a pointer by the garbage collector.
   INTEGER means that this is a Integer node and should be treated specially
   by the GC (because we need to copy the chain).
   ARRAY means that this is an array and should be treated specially by the GC.
   here CI_NO_PTRS is set if the array contains no pointers
*/
enum { CI_NONE = 0x00, CI_NO_PTRS = 0x01, CI_INTEGER = 0x02, CI_ARRAY = 0x04, CI_FOREIGN_PTR = 0x08,
       CI_PROC_STACK = 0x10, CI_MVAR = 0x20, CI_EXCEPTION_HANDLER = 0x40 };

/* constructor information */
typedef struct _CInfo {
  Info            info;
  HUInt           size;          /* size of the node in words */
  struct _Module* module;        /* the module this FInfo was loaded from */
  Char*           name;          /* name of the constructor */
  HUInt           number;        /* tag number */
  HUInt           flags;         /* special flags */

#ifdef HAT
  HNode           hatNode;
  HInfo           hatInfo;
#endif
}CInfo;

/*----------------------------------------------------------------------------------------------------*/

Int node_size(Node* p, Int* fstArg, Int* lstArg, Bool* hasPtrs);
void run(Node* node);

/*----------------------------------------------------------------------------------------------------*/

/* declares all the bytecode constants */
enum {
#  define op(n,x) x=n,
#  define op1(n,x) x=n,
#  define op2(n,x) x=n,
#  define op1S(n,x) x=n,
#  define op2S(n,x) x=n,
#  define op1_1(n,x) x=n,
#  define op2_1(n,x) x=n,
#  define opT(n,x) x=n,
#  define opL(n,x) x=n,
#  define opJ(n,x) x=n,
#  include "bytecodes.h"
#  undef op
#  undef op1
#  undef op2
#  undef op1S
#  undef op2S
#  undef op1_1
#  undef op2_1
#  undef opT
#  undef opL
#  undef opJ
};

/* the list of bytecode names - UNUSED? */
extern char* G_bytecodes[];

#endif
