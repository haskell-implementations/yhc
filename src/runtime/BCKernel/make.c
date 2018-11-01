#include "heap.h"
#include "primitive.h"
#include "integer.h"

Node* make_tuple(Node* fst, Node* snd){
  Global gFst = { NULL, &fst };
  Global gSnd = { NULL, &snd };
  Node* ret;

  REMOVE_IND(fst, Node*);
  REMOVE_IND(snd, Node*);

  heap_pushGlobal(&gFst);
  heap_pushGlobal(&gSnd);

  ret = (Node*)heap_alloc(wordsof(Node) + 2);
  MAKE_NODE(ret, G_infoTuple[2], N_NORMAL);
  INIT_HATNODE(ret, NULL);

  heap_popGlobal();
  heap_popGlobal();

  ret->args[0] = fst;
  ret->args[1] = snd;
  return ret;
}

Node* make_nTuple(int num, ...){
    va_list va;
    Node* ret;
    Node* args[MAX_TUPLE_SIZE];
    Global globs[MAX_TUPLE_SIZE];
    int i;

    /* special case, for the 0 tuple just return unit */
    if (num == 0){
        return G_nodeUnit;
    }

    /* extract arguments */
    assert(num > 0 && num < MAX_TUPLE_SIZE);
    va_start(va, num);
    for (i = 0; i < num; i++){
        /* extract argument */
        args[i] = va_arg(va, Node*);
        REMOVE_IND(args[i], Node*);

        /* push as global */
        globs[i].next = NULL;
        globs[i].global = &args[i];
        heap_pushGlobal(&globs[i]);
    }
    va_end(va);

    /* allocate memory */
    ret = (Node*)heap_alloc(wordsof(Node) + num);
    MAKE_NODE(ret, G_infoTuple[num], N_NORMAL);
    INIT_HATNODE(ret, NULL);

    /* fill arguments */
    for (i = 0; i < num; i++){
        ret->args[i] = args[i];
        heap_popGlobal();
    }
    /* return */
    return ret;
}

Node* make_cons(Node* head, Node* tail){
  Global gHead = { NULL, &head };
  Global gSnd = { NULL, &tail };
  Node* ret;

  REMOVE_IND(head, Node*);
  REMOVE_IND(tail, Node*);

  heap_pushGlobal(&gHead);
  heap_pushGlobal(&gSnd);

  ret = (Node*)heap_alloc(wordsof(Node) + 2);
  MAKE_NODE(ret, G_infoCons, N_NORMAL);
  INIT_HATNODE(ret, NULL);

  heap_popGlobal();
  heap_popGlobal();

  ret->args[0] = head;
  ret->args[1] = tail;
  return ret;
}

Node* make_int(Int value){
  INode* ret;

  ret = (INode*)heap_alloc(wordsof(INode));
  MAKE_NODE(ret, &G_infoInt, N_NORMAL);
  INIT_HATNODE(ret, NULL);
  ret->value = value;
  return (Node*)ret;
}

Node* make_integer(Int value){
  IntegerNode* ret;
#ifdef NO_LIBGMP
  Int extra = 0;
#else
  Int extra = value == 0 ? 0 : 1;
#endif
  ret = (IntegerNode*)heap_alloc(wordsof(IntegerNode) + extra);
  MAKE_NODE(ret, &G_infoInteger, N_NORMAL);
  INIT_HATNODE(ret, NULL);
#ifdef NO_LIBGMP
  ret->value.value = value;
#else
  if (value == 0){
    ret->value.size = 0;
  }else if (value < 0){
    ret->value.size = -1;
    ret->value.data[0] = -value;
  }else{
    ret->value.size = 1;
    ret->value.data[0] = value;
  }
#endif
  return (Node*)ret;
}

Node* make_integer_32(Int size, UInt32 low, UInt32 high){
  IntegerNode* ret;
#ifdef NO_LIBGMP
  Int extra = 0;
#else
  Int extra = ABS(size);
#endif
  ret = (IntegerNode*)heap_alloc(wordsof(IntegerNode) + extra);
  MAKE_NODE(ret, &G_infoInteger, N_NORMAL);
  INIT_HATNODE(ret, NULL);
#ifdef NO_LIBGMP
  ret->value.value = (Word)(((UInt64)high) << 32 | (UInt64)low);
#else
  ret->value.size = size;
  if (sizeof(Int32) == sizeof(Word)){
    ret->value.data[0] = low;
    ret->value.data[1] = high;
  }else if (sizeof(Int32) * 2 == sizeof(Word)){
    ret->value.data[0] = (Word)(((UInt64)high) << 32 | (UInt64)low);
  }else{
    abort();
  }
#endif
  return (Node*)ret;
}

Node* make_integer_64(Int sign, UInt64 value){
  IntegerNode* ret;
#ifdef NO_LIBGMP
  Int extra = 0;
#else
  Int extra = wordsof(Int64);
#endif

  ret = (IntegerNode*)heap_alloc(wordsof(IntegerNode) + extra);
  MAKE_NODE(ret, &G_infoInteger, N_NORMAL);
  INIT_HATNODE(ret, NULL);
#ifdef NO_LIBGMP
  ret->value.value = (Int64)value;
#else
  ret->value.size = (sign == 1) ? -extra : extra;
  if (sizeof(Int32) == sizeof(Word)){
    ret->value.data[0] = (Word)(value         & 0xFFFFFFFF);
    ret->value.data[1] = (Word)((value >> 32) & 0xFFFFFFFF);
  }else if (sizeof(Int32) *2 == sizeof(Word)){
    ret->value.data[0] = (Word)value;
  }else{
    abort();
  }
#endif
  return (Node*)ret;
}

Node* make_float(Float value){
  FloatNode* ret;

  ret = (FloatNode*)heap_alloc(wordsof(FloatNode));
  MAKE_NODE(ret, &G_infoFloat, N_NORMAL);
  INIT_HATNODE(ret, NULL);
  ret->value = value;
  return (Node*)ret;
}

Node* make_double(Double value){
  DoubleNode* ret;

  ret = (DoubleNode*)heap_alloc(wordsof(DoubleNode));
  MAKE_NODE(ret, &G_infoDouble, N_NORMAL);
  INIT_HATNODE(ret, NULL);
  ret->value = value;
  return (Node*)ret;
}

Node* make_char(Char value){
  INode* ret;

  ret = (INode*)heap_alloc(wordsof(INode));
  MAKE_NODE(ret, &G_infoChar, N_NORMAL);
  INIT_HATNODE(ret, NULL);
  ret->value = (Int)value;
  return (Node*)ret;
}

Node* make_array(Int size, Node* value){
  ArrayNode* ret;
  Global gValue = { NULL, &value };
  Int i;

  if (value != NULL){
    heap_pushGlobal(&gValue);
  }
  ret = (ArrayNode*)heap_alloc(wordsof(ArrayNode) + size);
  if (value != NULL){
    heap_popGlobal();
  }

  MAKE_NODE(ret, &G_infoArray, N_NORMAL);
  INIT_HATNODE(ret, NULL);
  ret->size = size;
  if (value != NULL){
    for (i = 0; i < size; i++){
      ret->data[i] = (Word)value;
    }
  }
  return (Node*)ret;
}

Node* make_string(const char* s){
  StringNode* ret;

  ret = (StringNode*)heap_alloc(wordsof(StringNode));
  MAKE_NODE(ret, &G_infoString, N_NORMAL);
  INIT_HATNODE(ret, NULL);
  ret->string = (char*)s;
  return (Node*)ret;
}

Node* make_just(Node* value){
  Node* ret;
  Global gValue = { NULL, &value };

  heap_pushGlobal(&gValue);
  ret = (Node*)heap_alloc(wordsof(Node)+1);
  heap_popGlobal();

  MAKE_NODE(ret, G_infoJust, N_NORMAL);
  INIT_HATNODE(ret, NULL);
  ret->args[0] = value;
  return ret;
}
