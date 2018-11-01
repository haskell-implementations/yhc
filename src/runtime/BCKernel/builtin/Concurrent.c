#include "../process.h"
#include "../make.h"
#include "../hsffi.h"

/*------------------------------------------------------------------------------------------------------------
  processes
  -----------------------------------------------------------------------------------------------------------*/

/* spawn a new process, the node has one argument which is a box with the closure to evaluate inside */
Node* primSpawnProcess(Node* node){
  Node* box = node->args[0];
  Node* eval;
  Process* proc;

  REMOVE_IND(box, Node*);
  eval = box->args[0];
  REMOVE_IND(eval, Node*);
  proc = proc_spawn(eval);
  return make_int((Int)proc->id);
}

/* kill the current process */
Node* primKillProcess(Node* node){
  proc_kill(G_proc);
  return NULL; /* nothing to return (ever) */
}

/* return the current thread id */
Node* primMyThreadId(Node* node){
  return make_int((Int)G_proc->id);
}

#if 1
/* a process that always spin locks, not very useful except for testing */
Node* primSpinLockProcess(Node* node){
  abort();
  return NULL;
}
#endif

/*------------------------------------------------------------------------------------------------------------
  mvars
  -----------------------------------------------------------------------------------------------------------*/

/* create a new empty mvar */
Node* primNewMVar(Node* node){
  MVarNode* mv = (MVarNode*)heap_alloc(wordsof(MVarNode));
  MAKE_NODE(mv, &G_infoMVar, N_NORMAL);
  proc_initMVar(mv);
  return (Node*)mv;
}

/* put a value into an mvar */
Node* primPutMVar(Node* node){
  MVarNode* mv = (MVarNode*)node->args[0];
  Node* value = node->args[1];

  REMOVE_IND(mv, MVarNode*);
  REMOVE_IND(value, Node*);

  if (mv->value == MVAR_EMPTY){
    /* mvar is empty give a value and unblock waiting processes */
    mv->value = value;
    proc_unblockMVar(mv, false);
    return G_nodeTrue;
  }else{
    /* return false because it's an error */
    return G_nodeFalse; /* nothing to return yet */
  }
}

/* take the value from an mvar */
Node* primTakeMVar(Node* node){
  MVarNode* mv = (MVarNode*)node->args[0];
  Node* value;

  REMOVE_IND(mv, MVarNode*);

  if (mv->value == MVAR_EMPTY){
    /* nothing to take, block */
    proc_blockMVar(mv, false);
    return NULL; /* nothing to return yet */
  }else{
    /* get the value */
    value = mv->value;
    mv->value = MVAR_EMPTY;
    return value;
  }
}

/* try and take the value from an mvar, but don't block */
Node* primTryTakeMVar(Node* node){
   MVarNode* mv = (MVarNode*)node->args[0];

   REMOVE_IND(mv, MVarNode*);
   if (mv->value == MVAR_EMPTY){
      return G_nodeNothing;
   }else{
      Node* value = mv->value;
      mv->value = MVAR_EMPTY;
      return make_just(value);
   }
}

/* read the value from an mvar, but don't take it */
Node* primReadMVar(Node* node){
  MVarNode* mv = (MVarNode*)node->args[0];
  Node* value;

  REMOVE_IND(mv, MVarNode*);

  if (mv->value == MVAR_EMPTY){
    /* nothing to take, block */
    proc_blockMVar(mv, false);
    return NULL; /* nothing to return yet */
  }else{
    /* get the value */
    value = mv->value;
    return value;
  }
}

/* swap the value of an mvar with another */
Node* primSwapMVar(Node* node){
  MVarNode* mv = (MVarNode*)node->args[0];
  Node* value = node->args[1];

  REMOVE_IND(mv, MVarNode*);
  REMOVE_IND(value, Node*);

  if (mv->value == MVAR_EMPTY){
    /* no value there so block */
    proc_blockMVar(mv, false);
    return NULL; /* nothing to return yet */
  }else{
    /* value already there so swap it */
    Node* ret = mv->value;
    mv->value = value;
    return ret;
  }
}

/* return whether an mvar is empty */
Node* primIsEmptyMVar(Node* node){
  MVarNode* mv = (MVarNode*)node->args[0];
  Node* result;
  REMOVE_IND(mv, MVarNode*);
  BOX_BOOL(result, mv->value == MVAR_EMPTY);
  return result;
}

/*-------------------------------------------------------------------------------------------------------------
  init
  ----------------------------------------------------------------------------------------------------------*/

void primConcurrent_init(){
  prim_addFun("primSpawnProcess", primSpawnProcess);
  prim_addFun("primKillProcess", primKillProcess);
  prim_addFun("primSpinLockProcess", primSpinLockProcess);
  prim_addFun("primMyThreadId", primMyThreadId);

  prim_addFun("primNewMVar", primNewMVar);
  prim_addFun("primPutMVar", primPutMVar);
  prim_addFun("primTakeMVar", primTakeMVar);
  prim_addFun("primReadMVar", primReadMVar);
  prim_addFun("primSwapMVar", primSwapMVar);
  prim_addFun("primIsEmptyMVar", primIsEmptyMVar);
}




