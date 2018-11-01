#ifndef __make_h__
#define __make_h__

Node* make_tuple(Node* fst, Node* snd);
Node* make_nTuple(int num, ...);

Node* make_cons(Node* head, Node* tail);
Node* make_int(Int value);
Node* make_integer(Int value);
Node* make_integer_32(Int value, UInt32 low, UInt32 high);
Node* make_integer_64(Int sign, UInt64 value);
Node* make_float(Float value);
Node* make_double(Double value);
Node* make_char(Char value);
Node* make_array(Int size, Node* val);
Node* make_string(const char* s);
Node* make_just(Node* val);

#endif
