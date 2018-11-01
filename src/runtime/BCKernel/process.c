#include "process.h"
#include "primitive.h"
#include "hsffi.h"
#include "thread.h"
#include "main.h"

#define _DEBUG_ 0

#if _DEBUG_
# define SHOW(x) x
#else
# define SHOW(x)
#endif

/* list of free processes */
static Process* G_freeProcList = NULL;

/* list of all processes */
Process* G_procList = NULL;

/* the current process */
Process* G_proc = NULL;

/* the current process ID */
static ProcessID G_nextID = 0;

/* the number of instructions before a swith */
Int G_insBeforeSwitch = 0;

/* whether process switching is disabled */
Bool G_procSwitchDisabled = false;

/* the list of processes that are ready */
static Process*  G_firstReady = NULL;
static Process*  G_lastReady = NULL;

/* the list of processes blocked waiting for exceptions to become available */
static Process*  G_excepBlockList = NULL;
Bool             G_excepBlocked = false;

/* return whether this is the only process */
Bool proc_isOnlyProcess(){
  assert(G_procList != NULL);
  return G_procList->linkNext == NULL;
}

/* allocate a new process */
Process* proc_alloc(){
  Process* proc;
  ProcStackNode* stack;

  /* get a new process */
  if (G_freeProcList != NULL){
    proc = G_freeProcList;
    G_freeProcList = proc->next;
  }else{
    proc = (Process*)malloc(sizeof(Process));
  }
  /* allocate a stack node */
  stack = (ProcStackNode*)heap_alloc(wordsof(ProcStackNode) + DEFAULT_STACK_SIZE);
  MAKE_NODE(stack, &G_infoProcStack, N_NORMAL);
  stack->size = DEFAULT_STACK_SIZE;
  stack->parent = proc;
  stack->exceptionStack = (ExceptionHandlerNode*)G_nodeUnit; /* keeps the GC much happier than using NULL */

  /* initialize it */
  proc->info.tag = I_TINFO;
  proc->id = G_nextID++;
  proc->pmode = PM_READY;
  proc->stack = stack;
  proc->blockedOn = G_nodeUnit;
  proc->waitContext = NULL;
  proc->saveFP = NULL;
  proc->isInterruptible = false;

  /* add to process list */
  proc->linkPrev = NULL;
  proc->linkNext = G_procList;
  if (G_procList){
    G_procList->linkPrev = proc;
  }
  G_procList = proc;

  /* setup counter */
  return proc;
}

/* move a stack from one place to another, watch out, these might overlap! */
void proc_moveStack(ProcStackNode* old, ProcStackNode* new, Bool newInit){
  Process* proc = old->parent;
  Word* oldEnd;
  Word* newEnd;
  UInt used;
  Frame* fp;

  SHOW(printf("moving stack for process %ld(%p) from %p to %p\n", proc->id, proc, old, new));

  /* setup some fast access pointers */
  oldEnd = &old->data[old->size];

  /* calculate ammount used and copy data across */
  used = oldEnd - (Word*)proc->saveFP;

  /* copy data across */
  if (newInit){
    /* called from GC to shift a stack across */
    memmove(new, old, sizeof(ProcStackNode) + old->size*sizeof(Word));
    newEnd = &new->data[new->size];
  }else{
    /* called from resizeStack with new stack */
    MAKE_NODE(new, &G_infoProcStack, N_NORMAL);
    new->parent = old->parent;
    new->exceptionStack = old->exceptionStack;
    newEnd = &new->data[new->size];
    memcpy(newEnd - used, proc->saveFP, used * sizeof(Word));
  }

  /* fix the saveFP */
  fp = (Frame*)(newEnd - used);
#if 0
  printf("oldFp = %d, newFp = %d\n", oldEnd - (Word*)proc->saveFP, newEnd - (Word*)fp);
  {
    int i;
    for (i = 0; i < used; i++){
      printf("%d.\t*%08p = %08p\t*%08p = %08p\n", i, &oldEnd[-i-1], oldEnd[-i-1], &newEnd[-i-1], newEnd[-i-1]);
    }
  }
  printf("proc %ld's saveFP was %p, is now %p\n", proc->id, proc->saveFP, fp);
#endif
  proc->saveFP = fp;

  /* go down the new stack and fix the frame pointers */
  while (fp->fp){
    fp->fp = (Frame*)(newEnd - (oldEnd - (Word*)fp->fp));
    fp = fp->fp;
  }

  /* set new stack */
  proc->stack = new;
}

/* reallocate the stack
   this very likely will change G_fp and G_sp, so make sure they are
   saved and restored appropriately.
*/
void proc_resizeStack(UInt extra){
  UInt newSize;
  ProcStackNode* stack;
  UInt max = G_options.maxStackSize / WORD_BYTES;

  /* check we haven't run out of stack */
  if (G_proc->stack->size >= max){
    fprintf(stderr, "Process %d ran out of stack!\n", G_proc->id);
    fprintf(stderr, "\tCurrent maximum stack size %d bytes\n", G_options.maxStackSize);
    fprintf(stderr, "\tNeed to set a bigger stack size ...\n");
    exit(1);
  }
  newSize = MIN(max, (G_proc->stack->size + extra) * 2);

  SHOW(printf("*** proc: resize stack, new size = %d ***\n", newSize));

  /* allocate a new stack node */
  stack = (ProcStackNode*)heap_alloc(wordsof(ProcStackNode) + newSize);
  stack->size = newSize;

  /* move data across */
  proc_moveStack(G_proc->stack, stack, false);
}

/* print the ready list, for debugging only */
static void proc_printReady(){
  Process* p;
  printf("Ready: [ ");
  for (p = G_firstReady; p; p = p->next){
    printf("%d ", p->id);
  }
  printf("]");
}

/* retrieve the next ready process */
static Process* proc_getReady(){
  Process* ret = G_firstReady;
  if (ret){
    G_firstReady = ret->next;
    if (!G_firstReady){
      G_lastReady = NULL;
    }
  }
  SHOW(
    if (ret){
        printf("*** proc: process %d is removed from front of ready list! ***: ", ret->id);
        proc_printReady();
        printf("\n");
    }else{
        printf("*** proc: no process is ready! ***\n");
    }
  );
  return ret;
}

/* make a process ready */
void proc_makeReady(Process* p){
  p->pmode = PM_READY;
  p->next = NULL;
  if (G_lastReady){
    G_lastReady->next = p;
  }else{
    G_firstReady = p;
  }
  G_lastReady = p;

  SHOW(printf("*** proc: process %d is now ready!, mode=%d ***: ", p->id, p->pmode); proc_printReady(); printf("\n"));
}

/* make a process ready, and ensure that it will be the next process to run */
void proc_makeReadyHiPriority(Process* p){
   p->pmode = PM_READY;
   p->next = G_firstReady;
   if (!G_firstReady){
      G_lastReady = p;
   }
   G_firstReady = p;

   SHOW(printf("*** proc: process %d is now ready, hi-priority!, mode=%d ***\n", p->id, p->pmode); proc_printReady();
        printf("\n"));
}

/* remove a process from the ready list, if it's present */
static void proc_removeFromReady(Process* proc){
    Process* p;
    Process* prev = NULL;

    for (p = G_firstReady; p != NULL; p = p->next){
        if (p == proc){
            if (prev){
                prev->next = proc->next;
            }else{
                G_firstReady = proc->next;
            }
            if (!proc->next){
                G_lastReady = prev;
            }
            return;
        }
        prev = p;
    }
}

/* check for deadlock */
static void proc_checkDeadlock(){
  Process* proc;

  /* are there actually no processes left at all? */
  if (G_procList == NULL){
    /* not deadlock, infact normal termination */
    exit(0);
  }
  G_proc->pmode = PM_BLOCKED;
  /* check whether every process is blocked */
  for (proc = G_procList; proc; proc = proc->linkNext){
     if (proc->pmode != PM_BLOCKED){
       return;
     }
  }
  /* yup, we're deadlocked */
  fprintf(stderr, "ERROR: program has deadlocked!\n");
  proc_printAll();
  exit(1);
}

/* switch to another process, must be ready to run (unchecked) */
static void proc_switchTo(Process* next, PMode mode){
  SHOW(printf("*** proc: context switch from %d to %d, oldmode=%d, newmode=%d ***\n", G_proc->id, next->id, G_proc->pmode,
        mode));
  if (mode == PM_READY){
    /* put current process on ready list */
    proc_makeReady(G_proc);
  }
  /* make the next process run */
  G_proc = next;
  G_proc->pmode = PM_RUNNING;
  SHOW(printf("*** proc: process %d is now in mode %d ***\n", G_proc->id, G_proc->pmode));
}

/* context switch, make sure to call PRE_SWITCH before and POST_SWITCH afterwards because
   this assumes everything except fp is saved on the stack. */
void proc_switch(PMode mode){
  Process* next;

  /* give another instruction allowance */
  G_insBeforeSwitch = INS_PER_THREAD;

  /* check switching is not disabled */
  if (G_procSwitchDisabled){
    return;
  }

  /* set new mode, must be done before calling finaliseAllWaiting because that might change mode again */
  G_proc->pmode = mode;

  /* finalize any waiting ffi processes */
  while (true){
    hsffi_finalizeAllWaiting();

    next = proc_getReady();
    if (!next){
        if (mode == PM_READY){
            /* well then just don't reschedule anything */
            SHOW(printf("*** proc: context switch, no other process ***\n"));
            return;
        }else if (mode == PM_BLOCKED || mode == PM_DEAD){
            /* check for deadlock */
            proc_checkDeadlock();
        }
        /* otherwise we must be waiting on FFI calls */
        SHOW(printf("*** proc: context switch, waiting on FFI, mode=%d ***\n", mode));
        SHOW(proc_printAll());
        hsffi_waitForFFI();

        /* try again */
        continue;
    }
    /* otherwise just continue */
    break;
  }
  proc_switchTo(next, mode);
}

/* spawn a new process to evaluate the given closure node */
Process* proc_spawn(Node* eval){
  Process* proc = proc_alloc();
  Frame* fs;
  FInfo* finfo;
  Word* end;

  SHOW(printf("*** proc: %d has just been spawned ****\n", proc->id));

  /* make sure there are two frames on the top of the stack */
  finfo = PINFO_FINFO(NODE_INFO(eval));

  end = (Word*)&proc->stack->data[proc->stack->size];
  fs = (Frame*)(end - wordsof(Frame)*2);

  /* the top frame */
  fs[0].fp = &fs[1];
  fs[0].ip = finfo->code;
  fs[0].vapptr = eval;

  /* a dummy frame */
  fs[1].fp = NULL;
  fs[1].ip = NULL;
  fs[1].vapptr = NULL;

  /* make sure saveFP is correct */
  proc->saveFP = &fs[0];

  /* add process to the ready list */
  proc_makeReady(proc);
  return proc;
}

/* kill the given process */
void proc_kill(Process* proc){
  SHOW(printf("*** proc: process %d was killed! ***, mode=PM_DEAD\n", proc->id));

  /* add to free proc list */
  proc->next = G_freeProcList;
  proc->pmode = PM_DEAD;
  G_freeProcList = proc;

  /* remove from the list of all processes */
  if (proc->linkPrev){
    proc->linkPrev->linkNext = proc->linkNext;
  }else{
    G_procList = proc->linkNext;
  }
  if (proc->linkNext){
    proc->linkNext->linkPrev = proc->linkPrev;
  }

  /* switch to another one if necessary */
  if (proc == G_proc){
    proc_switch(PM_DEAD);
  }
}


/* block a process on a given blackholed node */
void proc_blockHoled(Node* hole){
  SHOW(printf("*** proc: process %d blocks on node %p, mode=BLOCKED ***\n", G_proc->id, hole));
  /* add process to chain */
  G_proc->next = (Process*)NODE_INFO(hole);
  G_proc->pmode = PM_BLOCKED;
  G_proc->blockedMode = BM_HOLE;
  G_proc->blockedOn = hole;
  MAKE_NODE(hole, (Info*)G_proc, N_HOLE);

  /* reschedule */
  proc_switch(PM_BLOCKED);
}

/* unblock any waiting processes */
void proc_unblockHoled(Node* hole){
  Process* p;
  Process* next;

  SHOW(printf("*** proc: node %p is updated, unblocking processes! ***\n", hole));
  /* unblock all the waiting processes */
  for (p = (Process*)NODE_INFO(hole); p->info.tag == I_TINFO; p = next){
    next = p->next;
    p->pmode = PM_READY;
    p->blockedOn = G_nodeUnit;
    proc_makeReady(p);
  }
}

/* initialize an MVar */
void proc_initMVar(MVarNode* mv){
  mv->value = MVAR_EMPTY;
  mv->firstOnFull = mv->lastOnFull = NULL;
}

/* block on an MVar */
void proc_blockMVar(MVarNode* mv, Bool onEmpty){
  SHOW(printf("*** proc: thread %d is blocked on mvar %p to become %s ***\n", G_proc->id, mv,
              onEmpty ? "empty" : "full"));
  /* add to the end of the waiting list (it's unfair if it's to the beginning) */
  /* then wait on the consumer list */
  if (mv->lastOnFull){
    mv->lastOnFull->next = G_proc;
  }else{
    mv->firstOnFull = G_proc;
  }
  mv->lastOnFull = G_proc;
  G_proc->next = NULL;
  G_proc->blockedMode = BM_MVAR;
  G_proc->blockedOn = (Node*)mv;
  /* reschedule */
  proc_switch(PM_BLOCKED);
  SHOW(proc_printAll());
}

/* release a thread for an MVar */
void proc_unblockMVar(MVarNode* mv, Bool isEmpty){
  Process* proc = NULL;

  SHOW(printf("*** proc: mv %p is unlocked by becoming %s ", mv, isEmpty ? "empty" : "full"));

  /* then release the processes waiting on ful */
  if (mv->firstOnFull){
    /* remove from the front of the waiting list */
    proc = mv->firstOnFull;
    mv->firstOnFull = proc->next;
    if (proc == mv->lastOnFull){
      mv->lastOnFull = NULL;
    }
    proc->next = NULL;
  }
  if (proc == NULL){
    /* no process waiting */
    SHOW(printf("but no processes waiting ***\n"));
    return;
  }
  SHOW(printf(", so process %d is released ***\n", proc->id));

  /* make the process ready */
  proc->blockedOn = G_nodeUnit;
  proc_makeReady(proc);

  SHOW(proc_printAll());
}

/* give a process an FFI result. This involves shifting the stack a little,
   it's somewhat nasty to say the least ...
   this is *REALLY* tightly integrated with PRIMITIVE in mutator.c */
void proc_ffiResult(Process* proc, Node* result){
  Node** sp = (Node**)proc->saveFP;
  Frame frame;

  SHOW(printf("*** proc: process %d receives FFI result %p ***\n", proc->id, result));

  /* save the top frame on the stack */
  frame = *(Frame*)sp;
  sp += FRAME_SIZE;

  /* push the result on the stack */
  *--sp = result;

  /* adjust the saved frame pointer */
  sp -= FRAME_SIZE;
  proc->saveFP = (Frame*)sp;

  /* copy the saved frame back (with minor modifications) */
  frame.ip++;            /* adjust frame to move to next instruction */
  *proc->saveFP = frame;

  /* this process is ready now */
  proc_makeReady(proc);
}

/* print the current status of all processes */
void proc_printAll(){
  Process* p;
  printf("--ProcessList:-------------------------\n");
  for (p = G_procList; p; p = p->linkNext){
    char* desc[] = { "DEAD", "RUNNING", "READY", "BLOCKED", "WAITING", "THROWING" };
    printf("  %ld. %s\n", p->id, desc[p->pmode]);
  }
  printf("---------------------------------------\n");
}

/* dump the stack of the current process */
void proc_dumpStack(){
  ProcStackNode* stack = G_proc->stack;
  Word* p;
  Int i;

  for (p = (Word*)G_proc->saveFP, i = 0; p < &stack->data[stack->size]; p++,i++){
    printf("%ld. \t*%p = %p\n", i, p, (void*)*p);
  }
  printf("saveFP = %p\n", G_proc->saveFP);
}

/* force a process to be ready, even if it's currently waiting on something
   returns true if the action completed or false if the current process should wait */
Bool proc_forceReady(Process* proc){
   if (proc->pmode == PM_WAITING){
        /* request of the ffi system that this process should be registered to wait on the ffi */
        hsffi_waitOnContext(proc->waitContext);
        /* return that this process is blocked */
        return false;
    }else if (proc->pmode == PM_THROWING){
        /* unregister the process from the list of waiters */
        hsffi_removeWaiter(proc->waitContext, proc);
        /* this process is now ready to go */
        return true;
    }else if (proc->pmode == PM_BLOCKED){
        if (proc->blockedMode == BM_MVAR){
            /* unlink this process from the mvar */
            MVarNode* mv = (MVarNode*)proc->blockedOn;
            Process* p;
            Process* prev = NULL;

            /* search all attached processes */
            for (p = mv->firstOnFull; p != NULL; p = p->next){
                if (p == proc){
                    if (prev){
                        prev->next = proc->next;
                    }else{
                        mv->firstOnFull = proc->next;
                    }
                    if (!proc->next){
                        mv->lastOnFull = prev;
                    }
                }
                prev = p;
            }
        }else if (proc->blockedMode == BM_HOLE){
            /* remove from the list of processes waiting */
            Process* p;
            Process* prev = NULL;
            Node* hole = proc->blockedOn;
            for (p = (Process*)NODE_INFO(hole); p->info.tag == I_TINFO; p = p->next){
                if (p == proc){
                    if (prev){
                        prev->next = proc->next;
                    }else{
                        MAKE_NODE(hole, (Info*)proc->next, N_HOLE);
                    }
                }
                prev = p;
            }
        }else if (proc->blockedMode == BM_EXCEP){
            /* this really shouldn't happen since forceReady is only called by THROW_TO,
               and how can throwing proceeed if exceptions are blocked? If exceptions aren't blocked
               then no process should be blocked on exceptions */
            abort();
        }
    }
    /* this process is ready to throwTo */
    return true;
}

/* find a process by an ID, returns NULL if none found */
Process* proc_findByID(ProcessID id){
    Process* p;

    for (p = G_procList; p != NULL; p = p->linkNext){
        if (p->id == id){
            return p;
        }
    }
    return NULL;
}

/* force a switch to a particular process */
void proc_forceSwitchTo(Process* p, PMode pmode){
    SHOW(printf("*** proc: forced a switch to process %d from process %d, mode = %d ***\n", p->id, G_proc->id, pmode));
    proc_removeFromReady(p);
    G_proc->pmode = pmode;
    proc_switchTo(p, pmode);
}

/*--------------------------------------------------------------------------------------------------------------------
   Exception blocking
 ---------------------------------------------------------------------------------------------------------------------*/

/* turn on exception blocking */
void proc_excepBlock(){
    SHOW(printf("*** proc: exceptions are blocked! ***\n"));
    G_excepBlocked = true;
}

/* turn off exception blocking */
void proc_excepUnblock(){
    Process* p;

    if (!G_excepBlocked){
        return;
    }
    SHOW(printf("*** proc: exceptions are un-blocked! ***\n"));
    G_excepBlocked = false;

    /* release all waiting processes */
    for (p = G_excepBlockList; p != NULL; p = p->next){
        proc_makeReady(p);
    }
    G_excepBlockList = NULL;
}

/* block a process on exception blocking */
void proc_blockOnExcep(Process* p){
    if (!G_excepBlocked){
        return;
    }
    SHOW(printf("*** proc: process %d blocks on exceptions ***\n", p->id));
    p->next = G_excepBlockList;
    G_excepBlockList = p;
    p->blockedMode = BM_EXCEP;
}

