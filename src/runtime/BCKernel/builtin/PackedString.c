#include "../hsffi.h"

/* create a packed string given an array of characters */
Node* primPackString(Node* node){
  Node* len = (Node*)node->args[0];
  Node* cons = node->args[1];
  Global gCons = { NULL, &cons };
  PackedStringNode* res;
  Char* p;
  Int i, iLen;
  Int words;

  REMOVE_IND(len, Node*);
  iLen = ((INode*)len)->value;
  words = (iLen + WORD_BYTES) & ~WORD_BYTES_MASK;

  heap_pushGlobal(&gCons);
  res = (PackedStringNode*)heap_alloc(wordsof(PackedStringNode) + words);
  heap_popGlobal();

  MAKE_NODE(res, &G_infoPackedString, N_NORMAL);

  for (i = 0, p = res->string; i < iLen; i++, p++){
    INode* cnode;

    REMOVE_IND(cons, Node*);
    cnode = (INode*)cons->args[0];
    REMOVE_IND(cnode, INode*);
    *p = (Char)cnode->value;
    cons = cons->args[1];
  }
  *p = '\0';
  return (Node*)res;
}

/* get the character at a particular index for a packed string */
Char primPSGetChar(PackedStringNode* ps, Int i){
  return ps->string[i];
}

/* compare two packed strings */
Int primComparePS(PackedStringNode* a, PackedStringNode* b){
  Int result = strcmp(a->string,b->string);
  return result;
}

void primPackedString_init(){
  prim_addFun("primPackString", primPackString);
  prim_addFun("primPSGetChar", primPSGetChar);
  prim_addFun("primComparePS", primComparePS);
}
