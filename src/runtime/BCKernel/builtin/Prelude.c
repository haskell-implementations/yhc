#include "../hsffi.h"
#include "../integer.h"
#include "../make.h"
#include <math.h>
#include <string.h>

/*-------------------------------------------------------------------------------------------------------------------
  helpers
  ------------------------------------------------------------------------------------------------------------------*/

#define signum(v) (((v) < 0) ? -1 : (((v) == 0) ? 0 : 1))

#define FROM_INTEGER(_name, _type, _info, _op) \
  Node* _name(Node* node){ \
    Node* in = node->args[0]; \
    Global gIn = { NULL, &in };  \
    _type* ret; \
    mpz_t tmp; \
  \
    REMOVE_IND(in, Node*); \
  \
    heap_pushGlobal(&gIn); \
    ret = (_type*)heap_alloc(wordsof(_type)); \
    heap_popGlobal(); \
  \
    MAKE_NODE(ret, &_info, N_NORMAL); \
    toGMPInteger((IntegerNode*)in, tmp); \
    ret->value = mpz_get_##_op(tmp); \
    return (Node*)ret; \
  }

/*-------------------------------------------------------------------------------------------------------------------
  Int routines
  ------------------------------------------------------------------------------------------------------------------*/

/* Prelude.hs:primIntAbs primitive 1 :: Int -> Int */
Node* primIntAbs(Node* node){
  INode* in = (INode*)node->args[0];

  REMOVE_IND(in, INode*);
  if (in->value >= 0){
    return (Node*)in;
  }
  return make_int(-in->value);
}

/* Prelude.hs:primIntSignum primitive 1 :: Int -> Int */
Node* primIntSignum(Node* node){
  INode* in = (INode*)node->args[0];
  REMOVE_IND(in, INode*);
  return make_int(signum(in->value));
}

/* Prelude.hs:primIntFromInteger primitive 1 :: Integer -> Int */
FROM_INTEGER(primIntFromInteger, INode, G_infoInt, si);

/* Prelude.hs:primIntegerFromInt primitive 1 :: Int -> Integer */
Node* primIntegerFromInt(Node* node){
  INode* in = (INode*)node->args[0];
  REMOVE_IND(in, INode*);
  return make_integer(in->value);
}

/*-------------------------------------------------------------------------------------------------------------------
  Float routines
  --------------------------------------------------------------------------------------------------------------------*/

#define MON_OP_F(__name,__fun)     \
  Node* primFloat##__name(Node* node){ \
    FloatNode* in = (FloatNode*)node->args[0]; \
    REMOVE_IND(in, FloatNode*); \
    return make_float((Float)__fun(in->value)); \
  }

/* Prelude.hs:primFloatExp primitive 1 :: Float -> Float */
MON_OP_F(Exp,exp);

/* Prelude.hs:primFloatLog primitive 1 :: Float -> Float */
MON_OP_F(Log,log);

/* Prelude.hs:primFloatSqrt primitive 1 :: Float -> Float */
MON_OP_F(Sqrt,sqrt);

/* Prelude.hs:primFloatSin primitive 1 :: Float -> Float */
MON_OP_F(Sin,sin);

/* Prelude.hs:primFloatCos primitive 1 :: Float -> Float */
MON_OP_F(Cos,cos);

/* Prelude.hs:primFloatTan primitive 1 :: Float -> Float */
MON_OP_F(Tan,tan);

/* Prelude.hs:primFloatASin primitive 1 :: Float -> Float */
MON_OP_F(ASin,asin);

/* Prelude.hs:primFloatACos primitive 1 :: Float -> Float */
MON_OP_F(ACos,acos);

/* Prelude.hs:primFloatATan primitive 1 :: Float -> Float */
MON_OP_F(ATan,atan);

/* Prelude.hs:primFloatPow primitive 2 :: Float -> Float -> Float */
Node* primFloatPow(Node* node){
  FloatNode* x = (FloatNode*)node->args[0];
  FloatNode* y = (FloatNode*)node->args[1];

  REMOVE_IND(x, FloatNode*);
  REMOVE_IND(y, FloatNode*);
  return make_float((Float)pow(x->value, y->value));
}

/* Prelude.hs:primFloatFromInteger primitive 1 :: Integer -> Float */
float mpz_get_f(mpz_t mpz){
  return (float)mpz_get_d(mpz);
}

FROM_INTEGER(primFloatFromInteger, FloatNode, G_infoFloat, f);

/* Prelude.hs:primFloatAbs primitive 1  :: Float -> Float  */
Node* primFloatAbs(Node* node){
  FloatNode* in = (FloatNode*)node->args[0];

  REMOVE_IND(in, FloatNode*);
  if (in->value >= 0){
    return (Node*)in;
  }
  return make_float(-in->value);
}

/* Prelude.hs:primFloatSignum primitive 1  :: Float -> Float  */
MON_OP_F(Signum,signum);

/* Prelude.hs:primDecodeFloat primitive 1 :: Float -> (Integer,Int) */
Node* primDecodeFloat(Node* node){
  FloatNode* f = (FloatNode*)node->args[0];
  Int exp;
  IntegerNode* mant;
  //  mpz_t mpz;

  REMOVE_IND(f, FloatNode*);
  mant = decodeFloat32(f->value, &exp);

  //  toGMPInteger(mant, mpz);
  //  gmp_printf("decodeFloat %g = %Zd * 2^%d\n", f->value, mpz, exp);

  return make_tuple((Node*)mant, make_int(exp));
}

/* Prelude.hs:primEncodeFloat primitive 2 :: Integer -> Int -> Float */
Node* primEncodeFloat(Node* node){
  IntegerNode* man = (IntegerNode*)node->args[0];
  INode* exp = (INode*)node->args[1];
  Float f;

  REMOVE_IND(man, IntegerNode*);
  REMOVE_IND(exp, INode*);

  f = encodeFloat32(&man->value, exp->value);
  return make_float(f);
}

/*-------------------------------------------------------------------------------------------------------------------
  Double routines
  --------------------------------------------------------------------------------------------------------------------*/

#define MON_OP_D(__name,__fun)     \
  Node* primDouble##__name(Node* node){ \
    DoubleNode* in = (DoubleNode*)node->args[0]; \
    REMOVE_IND(in, DoubleNode*); \
    return make_double(__fun(in->value)); \
  }

/* Prelude.hs:primDoubleExp primitive 1 :: Double -> Double */
MON_OP_D(Exp,exp);

/* Prelude.hs:primDoubleLog primitive 1 :: Double -> Double */
MON_OP_D(Log,log);

/* Prelude.hs:primDoubleSqrt primitive 1 :: Double -> Double */
MON_OP_D(Sqrt,sqrt);

/* Prelude.hs:primDoubleSin primitive 1 :: Double -> Double */
MON_OP_D(Sin,sin);

/* Prelude.hs:primDoubleCos primitive 1 :: Double -> Double */
MON_OP_D(Cos,cos);

/* Prelude.hs:primDoubleTan primitive 1 :: Double -> Double */
MON_OP_D(Tan,tan);

/* Prelude.hs:primDoubleASin primitive 1 :: Double -> Double */
MON_OP_D(ASin,asin);

/* Prelude.hs:primDoubleACos primitive 1 :: Double -> Double */
MON_OP_D(ACos,acos);

/* Prelude.hs:primDoubleATan primitive 1 :: Double -> Double */
MON_OP_D(ATan,atan);

/* Prelude.hs:primDoublePow primitive 2 :: Double -> Double -> Double */
Node* primDoublePow(Node* node){
  DoubleNode* x = (DoubleNode*)node->args[0];
  DoubleNode* y = (DoubleNode*)node->args[1];

  REMOVE_IND(x, DoubleNode*);
  REMOVE_IND(y, DoubleNode*);
  return make_double(pow(x->value, y->value));
}

/* Prelude.hs:primDoubleFromInteger primitive 1  :: Integer -> Double */
FROM_INTEGER(primDoubleFromInteger, DoubleNode, G_infoDouble, d);

/* Prelude.hs:primDoubleAbs primitive 1  :: Double -> Double */
Node* primDoubleAbs(Node* node){
  DoubleNode* in = (DoubleNode*)node->args[0];

  REMOVE_IND(in, DoubleNode*);
  if (in->value >= 0){
    return (Node*)in;
  }
  return make_double(-in->value);
}

/* Prelude.hs:primDoubleSignum primitive 1  :: Double -> Double */
MON_OP_D(Signum,signum);

/* Prelude.hs:primDecodeDouble primitive 1 :: Double -> (Integer,Int) */
Node* primDecodeDouble(Node* node){
  DoubleNode* d = (DoubleNode*)node->args[0];
  IntegerNode* mant;
  Int32 exp;
  //  mpz_t mpz;

  REMOVE_IND(d, DoubleNode*);
  mant = decodeFloat64(d->value, &exp);

  //  toGMPInteger(mant, mpz);
  //  gmp_printf("decodeDouble %g = %Zd * 2^%d\n", d->value, mpz, exp);

  return make_tuple((Node*)mant, make_int(exp));
}

/* Prelude.hs:primEncodeDouble primitive 2 :: Integer -> Int -> Double */
Node* primEncodeDouble(Node* node){
  IntegerNode* mant = (IntegerNode*)node->args[0];
  INode* exp = (INode*)node->args[1];
  Double d;

  REMOVE_IND(mant, IntegerNode*);
  REMOVE_IND(exp, INode*);
  d = encodeFloat64(&mant->value, exp->value);
  return make_double(d);
}


/*-------------------------------------------------------------------------------------------------------------------
  Integer routines
  --------------------------------------------------------------------------------------------------------------------*/

#define BIN_OP(_name, _op)   \
  Node* _name(Node* node){ \
    IntegerNode* ix = (IntegerNode*)node->args[0]; \
    IntegerNode* iy = (IntegerNode*)node->args[1]; \
    IntegerNode* ret; \
    mpz_t mx, my, tmp; \
  \
    REMOVE_IND(ix, IntegerNode*); \
    REMOVE_IND(iy, IntegerNode*); \
  \
    toGMPInteger(ix, mx); \
    toGMPInteger(iy, my); \
    mpz_init(tmp); \
    _op(tmp, mx, my); \
    ret = fromGMPInteger(tmp); \
    mpz_clear(tmp); \
    return (Node*)ret; \
  }

#define CMP_OP(_name, _op)   \
  Node* _name(Node* node){ \
    IntegerNode* ix = (IntegerNode*)node->args[0]; \
    IntegerNode* iy = (IntegerNode*)node->args[1]; \
    mpz_t mx, my; \
  \
    REMOVE_IND(ix, IntegerNode*); \
    REMOVE_IND(iy, IntegerNode*); \
    toGMPInteger(ix, mx); \
    toGMPInteger(iy, my); \
  \
    return (mpz_cmp(mx, my) _op 0) ? G_nodeTrue : G_nodeFalse; \
  }


/* Prelude.hs:primIntegerEq primitive 2 :: Integer -> Integer -> Bool */
CMP_OP(primIntegerEq, ==);

/* Prelude.hs:primIntegerNe primitive 2 :: Integer -> Integer -> Bool */
CMP_OP(primIntegerNe, !=);

/* Prelude.hs:primIntegerLt primitive 2 :: Integer -> Integer -> Bool */
CMP_OP(primIntegerLe, <=);

/* Prelude.hs:primIntegerLe primitive 2 :: Integer -> Integer -> Bool */
CMP_OP(primIntegerLt, <);

/* Prelude.hs:primIntegerGe primitive 2 :: Integer -> Integer -> Bool */
CMP_OP(primIntegerGe, >=);

/* Prelude.hs:primIntegerGt primitive 2 :: Integer -> Integer -> Bool */
CMP_OP(primIntegerGt, >);

/* Prelude.hs:primIntegerAdd primitive 2 :: Integer -> Integer -> Integer */
BIN_OP(primIntegerAdd, mpz_add);

/* Prelude.hs:primIntegerSub primitive 2 :: Integer -> Integer -> Integer */
BIN_OP(primIntegerSub, mpz_sub);

/* Prelude.hs:primIntegerMul primitive 2 :: Integer -> Integer -> Integer */
BIN_OP(primIntegerMul, mpz_mul);

/* Prelude.hs:primIntegerNeg primitive 1 :: Integer -> Integer  */
Node* primIntegerNeg(Node* node){
  IntegerNode* in = (IntegerNode*)node->args[0];
  IntegerNode* ret;
  mpz_t m, tmp;

  REMOVE_IND(in, IntegerNode*);

  toGMPInteger(in, m);
  mpz_init(tmp);
  mpz_neg(tmp, m);
  ret = fromGMPInteger(tmp);
  mpz_clear(tmp);
  return (Node*)ret;
}

/* Prelude.hs:primIntegerQuot primitive 2 :: Integer -> Integer -> Integer */
BIN_OP(primIntegerQuot, mpz_tdiv_q);

/* Prelude.hs:primIntegerRem primitive 2 :: Integer -> Integer -> Integer  */
BIN_OP(primIntegerRem, mpz_tdiv_r);

/* Prelude.hs:primIntegerQuotRem primitive 2 :: Integer -> Integer -> (Integer, Integer) */
Node* primIntegerQuotRem(Node* node){
    Node* quot;
    Node* rem;
    Global gQuot = { NULL, &quot };

    quot = primIntegerQuot(node);
    heap_pushGlobal(&gQuot);
    rem  = primIntegerRem(node);
    heap_popGlobal();
    return make_nTuple(2, quot, rem);
}

/*-------------------------------------------------------------------------------------------------------------------
  ErrNo routines
  --------------------------------------------------------------------------------------------------------------------*/

Node* primStrError(Node* node){
  INode* ncode = (INode*)node->args[0];

  REMOVE_IND(ncode, INode*);

  return make_string(strerror(ncode->value));
}

/*-------------------------------------------------------------------------------------------------------------------
  Unsafe coerce
  --------------------------------------------------------------------------------------------------------------------*/

Node* primUnsafeCoerce(Node* node){
  Node* arg = node->args[0];
  REMOVE_IND(arg, Node*);
  return arg;
}

/*-------------------------------------------------------------------------------------------------------------------
  init
  --------------------------------------------------------------------------------------------------------------------*/

void primPrelude_init(){
  prim_addFun("primIntAbs", primIntAbs);
  prim_addFun("primIntSignum", primIntSignum);
  prim_addFun("primIntFromInteger", primIntFromInteger);
  prim_addFun("primIntegerFromInt", primIntegerFromInt);

  prim_addFun("primFloatExp", primFloatExp);
  prim_addFun("primFloatLog", primFloatLog);
  prim_addFun("primFloatSqrt", primFloatSqrt);
  prim_addFun("primFloatSin", primFloatSin);
  prim_addFun("primFloatCos", primFloatCos);
  prim_addFun("primFloatTan", primFloatTan);
  prim_addFun("primFloatASin", primFloatASin);
  prim_addFun("primFloatACos", primFloatACos);
  prim_addFun("primFloatATan", primFloatATan);
  prim_addFun("primFloatPow", primFloatPow);
  prim_addFun("primFloatFromInteger", primFloatFromInteger);
  prim_addFun("primFloatAbs", primFloatAbs);
  prim_addFun("primFloatSignum", primFloatSignum);
  prim_addFun("primDecodeFloat", primDecodeFloat);
  prim_addFun("primEncodeFloat", primEncodeFloat);

  prim_addFun("primDoubleExp", primDoubleExp);
  prim_addFun("primDoubleLog", primDoubleLog);
  prim_addFun("primDoubleSqrt", primDoubleSqrt);
  prim_addFun("primDoubleSin", primDoubleSin);
  prim_addFun("primDoubleCos", primDoubleCos);
  prim_addFun("primDoubleTan", primDoubleTan);
  prim_addFun("primDoubleASin", primDoubleASin);
  prim_addFun("primDoubleACos", primDoubleACos);
  prim_addFun("primDoubleATan", primDoubleATan);
  prim_addFun("primDoublePow", primDoublePow);
  prim_addFun("primDoubleFromInteger", primDoubleFromInteger);
  prim_addFun("primDoubleAbs", primDoubleAbs);
  prim_addFun("primDoubleSignum", primDoubleSignum);
  prim_addFun("primDecodeDouble", primDecodeDouble);
  prim_addFun("primEncodeDouble", primEncodeDouble);

  prim_addFun("primIntegerEq", primIntegerEq);
  prim_addFun("primIntegerNe", primIntegerNe);
  prim_addFun("primIntegerLt", primIntegerLt);
  prim_addFun("primIntegerLe", primIntegerLe);
  prim_addFun("primIntegerGe", primIntegerGe);
  prim_addFun("primIntegerGt", primIntegerGt);
  prim_addFun("primIntegerAdd", primIntegerAdd);
  prim_addFun("primIntegerSub", primIntegerSub);
  prim_addFun("primIntegerMul", primIntegerMul);
  prim_addFun("primIntegerNeg", primIntegerNeg);
  prim_addFun("primIntegerQuotRem", primIntegerQuotRem);
  prim_addFun("primIntegerQuot", primIntegerQuot);
  prim_addFun("primIntegerRem", primIntegerRem);

  prim_addFun("primStrError", primStrError);

  prim_addFun("primUnsafeCoerce", primUnsafeCoerce);

}

