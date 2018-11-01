#ifndef __stable_h__
#define __stable_h__

#include "node.h"

typedef struct _StablePtr {
  struct _StablePtr*    next;
  struct _StablePtr*    prev;
  Node*                 node;
}StablePtr;

extern StablePtr*  G_stablePtrs;

void               stable_init();
void               stable_exit();
StablePtr*         stable_create(Node* p);
void               stable_free(StablePtr* p);

#endif
