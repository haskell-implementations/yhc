#include "node.h"
#include "heap.h"
#include "module.h"
#include "integer.h"
#include "process.h"
#include "hsffi.h"


CInfo* cinfo_alloc(Int size, Char* name, Int number, Int flags, Object* obj, Module* mod, HPos* pos){
  CInfo* cinfo;
  Node* zcon;

  cinfo = (CInfo*)malloc(sizeof(CInfo));
  G_staticAlloced += sizeof(CInfo);
  cinfo->info.tag = I_CINFO;
  cinfo->size = (HUInt)size;
  cinfo->module = mod;
  cinfo->name = name;
  cinfo->number = (HUInt)number;
  cinfo->flags = (HUInt)flags;

  if (obj){
    obj->node = NULL;
    if (cinfo->size == 0){
      zcon = (Node*)heap_alloc(wordsof(Node));
      MAKE_NODE(zcon, cinfo, N_NORMAL);
      INIT_HATNODE(zcon, pos);
      obj->node = zcon;
      obj->global.global = &obj->node;
      heap_pushGlobal(&obj->global);
    }
    obj->info = (Info*)cinfo;
  }
#ifdef HAT
  cinfo->hatInfo.tracePtr = 0;
  cinfo->hatInfo.module = mod->hatModule;
  cinfo->hatInfo.name = name;
  cinfo->hatInfo.flags = HIFL_NONE;
#endif
  return cinfo;
}

FInfo* finfo_alloc(Int arity, Int stack, Int flags, Char* name, Int codeSize, CodePtr code, Int numConsts,
                   UByte* constTypes, ConstItem* constTable, Object* obj, Module* mod,
                   HPos* pos){
  PInfo* pinfo;
  FInfo* finfo;
  Node* caf;
  Int i;

  pinfo = (PInfo*)malloc(sizeof(PInfo) * (arity+1) + sizeof(FInfo));
  G_staticAlloced += sizeof(PInfo) * (arity+1) + sizeof(FInfo);
  for (i = 0; i < (arity+1); i++){
    pinfo[i].info.tag = I_PINFO;
    pinfo[i].size = (QUInt)i;
    pinfo[i].need = (QUInt)(arity - i);
  }
  finfo = PINFO_FINFO(pinfo);
  finfo->info.tag = I_FINFO;
  finfo->papTable = pinfo;
  finfo->link = NULL;
  finfo->arity = (HUInt)arity;
  finfo->stack = (HUInt)stack;
  finfo->flags = flags;
  finfo->module = mod;
  finfo->name = name;
  finfo->codeSize = codeSize;
  finfo->code = code;
  finfo->numConsts = (HUInt)numConsts;
  finfo->constTypes = constTypes;
  finfo->constTable = constTable;

  if (obj){
    caf = (Node*)heap_alloc(wordsof(Node));
    MAKE_NODE(caf, pinfo, N_NORMAL);
    INIT_HATNODE(caf, pos);
    obj->info = (Info*)finfo;
    obj->node = caf;
    obj->global.global = &obj->node;
    heap_pushGlobal(&obj->global);
  }
#ifdef HAT
  finfo->hatNode.flags = HNFL_UNTRACED;
  finfo->hatNode.data = pos;

  finfo->hatInfo.tracePtr = 0;
  finfo->hatInfo.module = mod->hatModule;
  finfo->hatInfo.name = name;
  finfo->hatInfo.flags = HIFL_NONE  | (mod->hatModule->isTrusted ? HIFL_TRUSTED : 0);
#endif
  return finfo;
}


XInfo* xinfo_alloc(Int arity, Int stack, Char* name, FFIFunction* func, Object* obj, Module* mod, HPos* pos){
  PInfo* pinfo;
  XInfo* xinfo;
  Node* caf;
  Int i;

  pinfo = (PInfo*)malloc(sizeof(PInfo) * (arity+1) + sizeof(XInfo));
  G_staticAlloced += sizeof(PInfo) * (arity+1) + sizeof(XInfo);
  for (i = 0; i < (arity+1); i++){
    pinfo[i].info.tag = I_PINFO;
    pinfo[i].size = (UByte)i;
    pinfo[i].need = (UByte)(arity - i);
  }
  xinfo = (XInfo*)PINFO_FINFO(pinfo);

  xinfo->info.tag = I_XINFO;
  xinfo->papTable = pinfo;
  xinfo->module = mod;
  xinfo->name = name;
  xinfo->arity = (HUInt)arity;
  xinfo->ffiFunc = func;

  if (obj){
    caf = (Node*)heap_alloc(wordsof(Node));
    MAKE_NODE(caf, pinfo, N_NORMAL);
    INIT_HATNODE(caf, pos);

    obj->info = (Info*)xinfo;
    obj->node = caf;
    obj->global.global = &obj->node;
    heap_pushGlobal(&obj->global);
  }
#ifdef HAT
  xinfo->hatNode.flags = HNFL_UNTRACED;
  xinfo->hatNode.data = pos;

  xinfo->hatInfo.tracePtr = 0;
  xinfo->hatInfo.module = mod->hatModule;
  xinfo->hatInfo.name = name;
  xinfo->hatInfo.flags = HIFL_NONE  | (mod->hatModule->isTrusted ? HIFL_TRUSTED : 0);
#endif
  return xinfo;
}

Int node_size(Node* p, Int* fstArg, Int* lstArg, Bool* hasPtrs){
  Info* info = NODE_INFO(p);

  REMOVE_TINFO(info);

  *hasPtrs = true;
  *fstArg = 0;
  if (info->tag == I_CINFO){
    CInfo* cinfo = (CInfo*)info;
    if (cinfo->flags & CI_NO_PTRS){
      /* remember if something has no pointers */
      *hasPtrs = false;
    }
#ifndef NO_LIBGMP
    if (cinfo->flags & CI_INTEGER){
      /* integers have to be handled specially because they are arrays with a funny size */
      IntegerNode* in = (IntegerNode*)p;
      return wordsof(IntegerNode) - wordsof(Node) + ABS(in->value.size);
    }
#endif
    if (cinfo->flags & CI_ARRAY){
      /* arrays are also a special case */
      ArrayNode* arr = (ArrayNode*)p;
      Int size = wordsof(ArrayNode) - wordsof(Node) + arr->size;

      *fstArg = 1;
      *lstArg = size - 1;
      return size;
    }
    if (cinfo->flags & CI_PROC_STACK){
      /* Process stacks are variable sized and the first argument only is a pointer */
      ProcStackNode* psn = (ProcStackNode*)p;
      *fstArg = 0;
      *lstArg = 0;
      return wordsof(ProcStackNode) - wordsof(Node) + psn->size;
    }
    if (cinfo->flags & CI_MVAR){
      /* MVars have one Haskell pointer and lots of C pointers */
      *fstArg = 0;
      *lstArg = 0;
      return cinfo->size;
    }
    if (cinfo->flags & CI_EXCEPTION_HANDLER){
      /* Exception handlers have one Haskell pointer and lots of C stuff */
      *fstArg = 0;
      *lstArg = 1;
      return cinfo->size;
    }
    *lstArg = ((Int) cinfo->size) - 1;
    return cinfo->size;
  }else if (info->tag == I_PINFO){
    PInfo* pinfo = (PInfo*)info;
    *lstArg = ((Int) pinfo->size) - 1;
    return pinfo->size;
  }else{
    abort();
    return 0; /* never reached, remove VS warning */
  }
}

