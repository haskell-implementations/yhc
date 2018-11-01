#ifndef __integer_h__
#define __integer_h__

#include "node.h"

typedef struct _Integer {
#ifdef NO_LIBGMP
  Int64         value;
#else
  Int           size;
  Word          data[0];
#endif
}Integer;

#ifdef NO_LIBGMP
  typedef Integer      mpz_t[1];
#endif

typedef struct _IntegerNode {
  NodeHeader         node;
  Integer       value;
}IntegerNode;

Int  getMPZAlloc(mpz_t mpz);
void _toGMPInteger(Integer* in, mpz_t mpz);
void toGMPInteger(IntegerNode* inode, mpz_t mpz);
void _fromGMPInteger(mpz_t mpz, Integer* i);
IntegerNode* fromGMPInteger(mpz_t mpz);

Int64 integer_value_64(IntegerNode* node);
UInt64 integer_value_u64(IntegerNode* node);

IntegerNode* decodeFloat32(Float32 f, Int* exp);
Float32 encodeFloat32(Integer* man, Int exp);

IntegerNode* decodeFloat64(Float64 f, Int* exp);
Float64 encodeFloat64(Integer* man, Int exp);

//---------------------------------------------------------------------------------------------------------------------

#ifdef NO_LIBGMP

#define mpz_init(v)             { v[0].value = 0; }
#define mpz_clear(v)            mpz_init(v)
#define mpz_set_ui(v, ui)       { v[0].value = (UInt)ui; }
#define mpz_set_si(v, si)       { v[0].value = (Int)si; }
#define mpz_cmp(a, b)           ((Int)(a[0].value - b[0].value))

#define mpz_neg(r, a)           { r[0].value = -a[0].value; }
#define mpz_add(r, a, b)        { r[0].value = a[0].value + b[0].value; }
#define mpz_sub(r, a, b)        { r[0].value = a[0].value - b[0].value; }
#define mpz_mul(r, a, b)        { r[0].value = a[0].value * b[0].value; }
#define mpz_mul_ui(r, a, ui)    { r[0].value = a[0].value * ui; }
#define mpz_tdiv_q(r, a, b)     { r[0].value = a[0].value / b[0].value; }
#define mpz_tdiv_r(r, a, b)     { r[0].value = a[0].value % b[0].value; }

#define mpz_ui_pow_ui(r, x, y)  { r[0].value = (Int64)pow(x, y); }

#define gmp_printf(a)           { printf("%Ld",

#define mpz_get_d(a)            ((Double)a[0].value)
#define mpz_get_si(a)           ((Int)a[0].value)


#endif

#endif

