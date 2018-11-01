#include "../hsffi.h"
#include "../make.h"

Node* primNewIORefC(Node* node){
  Node* ioref_data = node->args[0];

  REMOVE_IND(ioref_data, Node*);

  return make_just(ioref_data);
}

Node* primReadIORefC(Node* node){
  Node* ioref = node->args[0];

  REMOVE_IND(ioref, Node*);

  return ioref->args[0];
}

Node* primWriteIORefC(Node* node){
  Node* ioref = node->args[0];
  Node* ioref_data = node->args[1];

  REMOVE_IND(ioref, Node*);
  REMOVE_IND(ioref_data, Node*);

  ioref->args[0] = ioref_data;

  return G_nodeUnit;
}

void primIORef_init(){
  prim_addFun("primNewIORefC", primNewIORefC);
  prim_addFun("primReadIORefC", primReadIORefC);
  prim_addFun("primWriteIORefC", primWriteIORefC);
}
