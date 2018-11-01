#include "stable.h"

/* the list of stable pointers */
StablePtr* G_stablePtrs = NULL;

/* initialize the stable pointer system */
void stable_init(){
  G_stablePtrs = NULL;
}

/* exit the stable pointer system */
void stable_exit(){
  StablePtr* p;
  StablePtr* next;

  for (p = G_stablePtrs; p; p = next){
    next = p->next;
    free(p);
  }
}

/* create a new stable pointer */
StablePtr* stable_create(Node* p){
  StablePtr* ret = (StablePtr*)malloc(sizeof(StablePtr));
  ret->node = p;
  ret->prev = NULL;
  ret->next = G_stablePtrs;

  if (G_stablePtrs){
    G_stablePtrs->prev = ret;
  }
  return ret;
}

/* free a stable pointer */
void stable_free(StablePtr* p){
  if (p->next){
    p->next->prev = p->prev;
  }
  if (p->prev){
    p->prev->next = p->next;
  }else{
    G_stablePtrs = p->next;
  }
  free(p);
}

