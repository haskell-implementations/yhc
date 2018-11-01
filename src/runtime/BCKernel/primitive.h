#ifndef __primitive_h__
#define __primitive_h__

#include "node.h"

/* load all the primitive functions and data types */
void    prim_load();
void    prim_addCon(Char* name, Int size, Int number, Int flags);
void    prim_addFun(Char* name, void* func);
void*   prim_lookupFun(Char* name);

/* primitive constructor infos defined inside the interpretter */
extern CInfo  G_infoInt;
extern CInfo  G_infoLong;
extern CInfo  G_infoFloat;
extern CInfo  G_infoDouble;
extern CInfo  G_infoChar;
extern CInfo  G_infoInteger;
extern CInfo  G_infoString;
extern CInfo  G_infoPackedString;
extern CInfo  G_infoBox;
extern CInfo  G_infoForeignPtr;
extern CInfo  G_infoCharArray;
extern CInfo  G_infoArray;
extern CInfo  G_infoProcStack;
extern CInfo  G_infoMVar;
extern CInfo  G_infoExceptionHandler;

/* primitive byte code function infos defined inside the interpretter */
extern FInfo* G_infoApply;
extern FInfo* G_infoPrimCString;
extern FInfo* G_infoCatch;

/* things defined in the prelude which are none the less essential */
extern Node*  G_nodeNil;
extern CInfo* G_infoCons;
extern CInfo* G_infoRight;
extern Node*  G_nodeUnit;
extern Node*  G_nodeTrue;
extern Node*  G_nodeFalse;
extern Node*  G_nodeNothing;
extern CInfo* G_infoJust;
extern Node*  G_nodeZapStack;
extern Node*  G_nodeZapArg;
extern Node*  G_nodeBlackHole;
extern Node*  G_nodeHputcOk;
extern FInfo* G_infoBlackHole;
extern CInfo* G_infoEBox;

/* tuples, because they're occasionally useful in the FFI */
#define MAX_TUPLE_SIZE          20      /* limited by the prelude */

extern CInfo* G_infoTuple[MAX_TUPLE_SIZE];

#endif
