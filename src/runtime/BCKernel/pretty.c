#include "platform.h"
#include "module.h"
#include "heap.h"
#include "integer.h"
#include "pretty.h"
#include "primitive.h"
#include "foreign.h"
#include "process.h"

char* G_bytecodes[] = {
#define ins(x) #x
#  include "_bytecodes.h"
#undef ins
};

void pr_binary(Int dig, UInt num){
  UInt quot,rem;
  if (dig == 0){
    return;
  }
  quot = num / 2;
  rem = num % 2;
  printf("%c", rem ? '1' : '0');
  if (dig % 4 == 1){
    printf(" ");
  }
  pr_binary(dig - 1, quot);
}

Int pr_ins_table(Int i, UInt8* code){
  UInt16* size = (UInt16*)(code + 1);
  UInt16* table = &size[1];
  Int j;

  printf("TABLE_SWITCH [%u] {", *size);
  for (j = 0; j < *size; j++){
    printf("%ld -> %lu", j, i + 1 + table[j]);
    if (j != *size - 1){
      printf(", ");
    }
  }
  printf("}");
  return 1 + 2 + *size * 2;
}

Int pr_ins_lookup(Char* ins, Int i, UInt8* code){
  UInt16* size = (UInt16*)(code + 1);
  UInt16* table = &size[2];
  UInt16 def = size[1];
  Int j;

  printf("%s [%u] { _ -> %ld, ", ins, size[0], i + 1 + def);
  for (j = 0; j < *size; j++){
    printf("%u -> %lu", table[j*2], i + 1 + table[j*2+1]);
    if (j != *size - 1){
      printf(", ");
    }
  }
  printf("}");
  return 1 + 4 + *size * 4;
}

Int pr_ins(Int i, UInt8* code){
#define ub *(UInt8*)&code[1]
#define us *(UInt16*)&code[1]
#define b  *(Int8*)&code[1]
#define s  *(Int16*)&code[1]
#define ub_2 *(UInt8*)&code[2]
#define ub_3 *(UInt8*)&code[3]

  if (*code == END_CODE){
    printf("END_CODE");
    return 0;
  }
  switch (*code){
#   define op(n,x)  case x: printf(#x); return 1;
#   define op1(n,x) case x: printf(#x " %u",ub); return 2;
#   define op2(n,x) case x: printf(#x " %u",us); return 3;
#   define op1S(n,x) case x: printf(#x " %d",b); return 2;
#   define op2S(n,x) case x: printf(#x " %d",s); return 3;
#   define op1_1(n,x) case x: printf(#x " %u %u", ub, ub_2); return 3;
#   define op2_1(n,x) case x: printf(#x " %d %u", us, ub_3); return 4;
#   define opT(n,x) case x: return pr_ins_table(i, code);
#   define opL(n,x) case x: return pr_ins_lookup(#x, i, code);
#   define opJ(n,x) case x: printf(#x " %lu", us + i); return 3;
#     include "bytecodes.h"
#   undef op
#   undef op1
#   undef op2
#   undef op1S
#   undef op2S
#   undef op1_1
#   undef op2_1
#   undef opT
#   undef opL
  default:
    printf("UNKNOWN %d", *code); return 1;
  }
#undef ub
#undef us
#undef b
#undef s
#undef ub_2
#undef ub_3
}

void pr_code(UInt8* code){
  Int z, i = 0;
  UInt8* p = code;

  for (;;){
    printf("\t\t%ld.\t[0x%02x] ", i, *p);
    z = pr_ins(i, p);
    printf("\n");
    if (!z){
      break;
    }
    p += z;
    i += z;
  }
}

void pr_info_short(Info* info){
  PInfo* pi = (PInfo*)info;
  switch (info->tag){
  case I_FINFO: printf("FInfo '%s'[%d]", ((FInfo*)info)->name, ((FInfo*)info)->arity); return;
  case I_CINFO: printf("CInfo '%s'", ((CInfo*)info)->name); return;
  case I_XINFO: printf("XInfo '%s'", ((XInfo*)info)->name); return;
  case I_PINFO: printf("PInfo '%s'(%d/%d)", PINFO_FINFO(pi)->name, pi->size, pi->need); return;
  default:
    printf("Unknown Info! %p[%d]", info, info->tag);
  }
}

void pr_constItem(UInt8 type, ConstItem item){
  switch (type){
  case C_SPACE: printf("SPACE"); return;
  case C_INFO: printf("INFO "); pr_info_short((Info*)item); return;
  case C_NODE: printf("NODE %p [", (Node*)item); pr_node((Node*)item,1);/*info_short(NODE_INFO(item));*/ printf("]"); return;
  case C_INT: printf("INT %ld", (Int)item); return;
  case C_FLOAT: printf("FLOAT %g", (Float)item); return;
  case C_DOUBLE: printf("DOUBLE ..."); return;
  case C_STRING: printf("STRING '%s'", ((StringNode*)item)->string); return;
#ifdef HAT
  case C_POS: {
    HPos* pos = (HPos*)item;
    printf("POS %s:%ld:%ld", pos->module->name, pos->start, pos->end);
    return;
  }
  case C_VAR_DESC: {
    HVarDesc* desc = (HVarDesc*)item;
    printf("VAR_DESC '%s' %s:%ld:%ld", desc->name, desc->pos.module->name, desc->pos.start, desc->pos.end);
    return;
  }
#endif
  default:
    printf("UNKNOWN!! %p", (void*)item);
    return;
  }
}

void pr_finfo(FInfo* info){
  Int i;
  printf("\tFInfo %s [ arity: %d, consts: %d ]{\n", info->name, info->arity, info->numConsts);
  pr_code(info->code);
  printf("\t\tconsts {\n");
  for (i = 0; i < info->numConsts; i++){
    printf("\t\t\t%ld.\t", i);
    pr_constItem(info->constTypes[i], info->constTable[i]);
    printf("\n");
  }
  printf("\t\t}\n");
  printf("\t}\n\n");
}

void pr_cinfo(CInfo* info){
  printf("\tCInfo %s [ size: %d, number: %d ]\n", info->name, info->size, info->number);
}

void pr_pinfo(PInfo* info){
  FInfo* finfo = PINFO_FINFO(info);
  printf("\tPInfo %s [ size: %d, need: %d ]\n", finfo->name, info->size, info->need);
}

void pr_xinfo(XInfo* info){
  printf("\tXInfo %s [ arity: %d, func: %p ]\n", info->name, info->arity, info->ffiFunc);
}

void pr_info(Info* info){
  switch (info->tag){
  case I_FINFO:
    pr_finfo((FInfo*)info);
    break;
  case I_PINFO:
    pr_pinfo((PInfo*)info);
    break;
  case I_CINFO:
    pr_cinfo((CInfo*)info);
    break;
  case I_XINFO:
    pr_xinfo((XInfo*)info);
    break;
  default:
    printf("Unknown info: tag = %d!!\n", info->tag);
  }
}

void pr_object(Object* obj){
  pr_info(obj->info);
}

void pr_module(Module* mod){
  Int i;

  printf("Module %s [objects: %ld, strings: %ld] {\n", mod->name, mod->numObjects, mod->numStrings);
  for (i = 0; i < (Int)mod->numObjects; i++){
    if (mod->objects[i].object != NULL){
      pr_object(mod->objects[i].object);
    }
  }
  printf("}\n");
}

void pr_string(Char* string){
  Char* p;

  for (p = string; *p; p++){
    if (*p >= ' ' && *p < 0x7f){
      if (*p == '\\'){
        printf("\\\\");
      }else{
        printf("%c", *p);
      }
    }else{
      printf("\\%02x", *p);
    }
  }
}

void pr_nodeWith(Node* head, Node** args, Int depth){
  Info* info;
  Int size = 0, i;

#if 0
    if (head < (Node*)G_hpStart || head > (Node*)G_hp){
      printf("???[%p,%p]", head,*(Node**)head);
      return;
    }
#endif

  info = NODE_INFO(head);

  REMOVE_TINFO(info);

  if (depth <= 0){
    printf("...");
    return;
  }
  if (info == (Info*)&G_infoInt){
    printf("(CInt %ld)", (Int)args[0]);
    return;
  }
  if (info == (Info*)&G_infoLong){
    Int64 l = *(Int64*)args;
    printf("(CLong %Ld)", l);
    return;
  }
  if (info == (Info*)&G_infoChar){
    Char c = (Char)((Word)args[0]);
    if (c >= ' ' && c < 0x7f){
      printf("(CChar '%c')", c);
    }else{
      printf("(CChar 0x%02x)", c);
    }
    return;
  }
  if (info == (Info*)&G_infoInteger){
    Integer* i = (Integer*)args;
    mpz_t tmp;

#ifdef NO_LIBGMP
    printf("(CInteger %Ld)", i->value);
#else
    _toGMPInteger(i, tmp);
    gmp_printf("(CInteger %Zd)",tmp);
#endif
    return;
  }
  if (info == (Info*)&G_infoFloat){
    Float f = (Float)((Word)args[0]);
    printf("(CFloat %g)", f);
    return;
  }
  if (info == (Info*)&G_infoDouble){
    Double d = *(Double*)args;
    printf("(CDouble %g)", d);
    return;
  }
  if (info == (Info*)&G_infoArray){
    ArrayNode* arr = (ArrayNode*)(args-1);
    printf("(CArray %ld [", arr->size);
    for (i = 0; i < (Int)arr->size; i++){
      printf(" ");
      pr_node((Node*)arr->data[0], depth-1);
    }
    printf(" ])");
    return;
  }
  if (info == (Info*)&G_infoString){
    char* string = (char*)args[0];
    printf("(CString '");pr_string(string);printf("')");
    return;
  }
  if (info == (Info*)&G_infoForeignPtr){
    ForeignPtr* fp = (ForeignPtr*)args[0];
    printf("(CForeignPtr %p [p%p m%s <%p >%p])",
           fp, fp->ptr, fp->marked ? "T":"F", fp->prev, fp->next);
    return;
  }
  if (info == (Info*)&G_infoBox){
    printf("(CBox %p)", args[0]);
    return;
  }
  if (info == (Info*)&G_infoMVar){
    Process* p;
    printf("(CMVar[%p] ", head);
    pr_node(args[0], depth-1);
    printf(" {");
    for (p = (Process*)args[1]; p; p = p->next){
      printf(" %ld", p->id);
    }
    printf("})");
    return;
  }
  if (info == (Info*)&G_infoExceptionHandler){
    printf("(CExceptionHandler ...)");
    return;
  }
  if (head == G_nodeZapStack || head == G_nodeZapArg){
    printf("ZAP");
    return;
  }
  switch (info->tag){
  case I_PINFO: {
    Int need = ((PInfo*)info)->need;
    FInfo* finfo = PINFO_FINFO(info);

    size = ((PInfo*)info)->size;
    if (NODE_FLAGS(head) == N_HOLE){
      printf("\033[4m");
    }
    printf("(F%s/%ld", finfo->name, need);
    if (NODE_FLAGS(head) == N_HOLE){
      printf("\033[24m");
    }
    break;
  }
  case I_CINFO:
    size = ((CInfo*)info)->size;
    printf("(C%s", ((CInfo*)info)->name);
    if (((CInfo*)info)->flags & CI_NO_PTRS){
      printf(" <<INTERNAL>>)");
      return;
    }
    break;
  default:
    printf("("); pr_info_short(info);printf(")");
    return;
  }
  for (i = 0; i < size; i++){
    printf(" ");
    pr_node(args[i], depth-1);
  }
  printf(")");
}


void pr_node(Node* node, Int depth){
  if (depth <= 0){
    printf("...");
    return;
  }
  if (node == 0){
    printf("NULL");
    return;
  }
  REMOVE_IND(node, Node*);
  pr_nodeWith(node, node->args, depth);
}



