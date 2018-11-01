#ifndef __process_h__
#define __process_h__

#include "node.h"
#include "heap.h"

/* the id of a process */
typedef UInt  ProcessID;

/* default process stack size, in words */
#define DEFAULT_STACK_SIZE     64

/* the number of instructions to run per thread */
#define INS_PER_THREAD         10000

/* process mode
    DEAD     - process is dead, can be recycled as a new one
    RUNNING  - process is currently running
    READY    - process is ready to run but is currently not scheduled
    BLOCKED  - process is unable to run because it's waiting on blackhole/mvar
    WAITING  - process is unable to run because it's waiting on an FFI action
    THROWING - process is unable to run because it's waiting to throw an exception
*/
typedef enum { PM_DEAD, PM_RUNNING, PM_READY, PM_BLOCKED, PM_WAITING, PM_THROWING } PMode;

/* blocked mode
    BM_HOLE - waiting on a blackhole
    BM_MVAR - waiting on an mvar
    BM_EXCEP - waiting for exceptions to unblock */
typedef enum { BM_HOLE, BM_MVAR , BM_EXCEP } BMode;

struct _Process;
struct _FFIContext;

/* a process stack node */
typedef struct _ProcStackNode {
  NodeHeader                node;
  ExceptionHandlerNode*     exceptionStack;  /* exception stack for this process */
  struct _Process*          parent;          /* the process that owns us */
  UInt                      size;            /* the size of the stack */
  Word                      data[0];         /* the stack data */
}ProcStackNode;

/* a process, is also a TInfo (used in blackhole-blocking) */
typedef struct _Process {
  Info                      info;            /* because it's also a TInfo */
  HUInt                     pmode;           /* current process mode */
  struct _Process*          linkNext;        /* next process in the list of all processes */
  struct _Process*          linkPrev;        /* prev process in the list of all processes */

  struct _Process*          next;            /* next in chain, use varies depending on process status */
  ProcessID                 id;              /* the unique id of the process */
  Frame*                    saveFP;          /* the saved frame pointer */
  ProcStackNode*            stack;           /* pointer to the stack node for the process */
  Node*                     blockedOn;       /* node this process is waiting on */
  BMode                     blockedMode;     /* what type of thing 'blockedOn' is */
  struct _FFIContext*       waitContext;     /* ffi context that we are waiting on */
  Bool                      isInterruptible; /* if true then this thread will accept throwTos even when blocked */
}Process;

/* an MVar node */
typedef struct _MVarNode {
  NodeHeader          node;
  Node*               value;

  /* consumers are waiting for the mvar to become full */
  Process*            firstOnFull;
  Process*            lastOnFull;
}MVarNode;

#define MVAR_EMPTY   G_nodeBlackHole

/* the current running process */
extern Process*      G_proc;

/* list of all processes */
extern Process*      G_procList;

/* the number of instructions left before a switch */
extern Int           G_insBeforeSwitch;

/* whether switching is disabled */
extern Bool          G_procSwitchDisabled;

/* whether exceptions are blocked */
extern Bool          G_excepBlocked;


Bool     proc_isOnlyProcess();
Process* proc_alloc();
Process* proc_spawn(Node* eval);
void     proc_kill(Process* proc);
void     proc_resizeStack(UInt extra);
void     proc_moveStack(ProcStackNode* old, ProcStackNode* new, Bool newInit);
void     proc_switch(PMode mode);
void     proc_blockHoled(Node* thunk);
void     proc_unblockHoled(Node* thunk);
void     proc_initMVar(MVarNode* mvar);
void     proc_blockMVar(MVarNode* mvar, Bool onEmpty);
void     proc_ffiResult(Process* proc, Node* result);
void     proc_unblockMVar(MVarNode* mvar, Bool isEmpty);
void     proc_printAll();
void     proc_dumpStack();
Bool     proc_forceReady(Process* proc);
Process* proc_findByID(ProcessID id);
void     proc_makeReady(Process* p);
void     proc_makeReadyHiPriority(Process* p);
void     proc_forceSwitchTo(Process* p, PMode mode);
void     proc_excepBlock();
void     proc_excepUnblock();
void     proc_blockOnExcep(Process* p);

#endif

