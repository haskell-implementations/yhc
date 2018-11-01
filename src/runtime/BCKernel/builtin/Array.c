#include "../hsffi.h"
#include "../make.h"

#include <string.h>

#define DEBUG 0

#if DEBUG
# define SHOW(x) x
#else
# define SHOW(x)
#endif

Node* primNewVectorC(Node* node){
  INode* size = (INode*)node->args[0];
  Node* box   = node->args[1];

  REMOVE_IND(size, INode*);
  REMOVE_IND(box, Node*);

  SHOW(printf("newArray %d %p\n", size->value, box->args[0]));
  return make_array(size->value, box->args[0]);
}

Node* primCopyVectorC(Node* node){
  Node* arr = node->args[0];
  ArrayNode* ret;
  Global gArr = { NULL, &arr };
  Int size;

  REMOVE_IND(arr, Node*);
  size = ((ArrayNode*)arr)->size;

  heap_pushGlobal(&gArr);
  ret = (ArrayNode*)make_array(size, NULL);
  heap_popGlobal();

  SHOW(printf("copyArray %p[%d]\n", arr, size));
  memcpy(ret->data, ((ArrayNode*)arr)->data, sizeof(Word) * size);
  return (Node*)ret;
}

Node* primVectorIndexC(Node* node){
  ArrayNode* arr = (ArrayNode*)node->args[0];
  INode* idx = (INode*)node->args[1];

  REMOVE_IND(arr, ArrayNode*);
  REMOVE_IND(idx, INode*);

  SHOW(printf("indexArray %p[%d] %d => %p\n",arr,arr->size,idx->value,arr->data[idx->value]);)
  return (Node*)arr->data[idx->value];
}

Node* primUpdateVectorC(Node* node){
  INode* idx = (INode*)node->args[0];
  Node* val  = node->args[1];
  ArrayNode* arr = (ArrayNode*)node->args[2];

  REMOVE_IND(idx, INode*);
  REMOVE_IND(val, Node*);
  REMOVE_IND(arr, ArrayNode*);

  arr->data[idx->value] = (Word)val->args[0];

  SHOW(printf("updateArray %p[%d] %d %p\n", arr, arr->size,idx->value, val));
  return G_nodeUnit;
}


void primArray_init(){
  prim_addFun("primNewVectorC", primNewVectorC);
  prim_addFun("primCopyVectorC", primCopyVectorC);
  prim_addFun("primVectorIndexC", primVectorIndexC);
  prim_addFun("primUpdateVectorC", primUpdateVectorC);
}
