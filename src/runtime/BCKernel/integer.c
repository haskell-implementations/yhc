#include "integer.h"
#include "heap.h"
#include "primitive.h"
#include "make.h"

/*------------------------------------------------------------------------------------------*/

/* functions to convert to and from GMP integers,
   NOTE: these abuse the internal format of GMP integers! */
Int getMPZAlloc(mpz_t mpz){
#ifdef NO_LIBGMP
  return 1;
#else
  return mpz[0]._mp_alloc;
#endif
}

void _toGMPInteger(Integer* in, mpz_t ret){
#ifdef NO_LIBGMP
  ret[0].value = in->value;
#else
  ret[0]._mp_alloc = ABS(in->size);
  ret[0]._mp_size = in->size;
  ret[0]._mp_d = (mp_limb_t*)in->data;
#endif
}

void toGMPInteger(IntegerNode* inode, mpz_t ret){
  _toGMPInteger(&inode->value, ret);
}

void _fromGMPInteger(mpz_t mpz, Integer* i){
#ifdef NO_LIBGMP
  i->value = mpz[0].value;
#else
  i->size = mpz[0]._mp_size;
  memcpy(i->data, mpz[0]._mp_d, sizeof(Word) * ABS(i->size));
#endif
}

IntegerNode* fromGMPInteger(mpz_t mpz){
  IntegerNode* ret;
#ifdef NO_LIBGMP
  Int aSize = 0;
#else
  Int aSize = ABS(mpz[0]._mp_size);
#endif

  ret = (IntegerNode*)heap_alloc(wordsof(IntegerNode) + aSize);
  MAKE_NODE(ret, &G_infoInteger, N_NORMAL);
  INIT_HATNODE(ret, NULL /* FIXME: sensible pos?? */);

  _fromGMPInteger(mpz, &ret->value);
  return ret;
}

Int64 integer_value_64(IntegerNode* node){
#ifdef NO_LIBGMP
  return node->value.value;
#else
  UInt64 u = integer_value_u64(node);
  return (node->value.size < 0) ? -(Int64)u : (Int64)u;
#endif
}

UInt64 integer_value_u64(IntegerNode* node){
  Integer* in = &node->value;

#ifdef NO_LIBGMP
  return (UInt64)in->value;
#else
  UInt64 ret;

  if (sizeof(Int32) == sizeof(Word)){
    if (in->size == 0){
      ret = 0;
    }else if (ABS(in->size) == 1){
      ret = in->data[0];
    }else{
      ret = (UInt64)in->data[0] | (((UInt64)in->data[1]) << 32);
    }
  }else if (sizeof(Int32) * 2 == sizeof(Word)){
    if (in->size == 0){
      ret = 0;
    }else{
      ret = in->data[0];
    }
  }else{
    abort();
  }

  return ret;
#endif
}

/*---------------------------------------------------------------------------------------------------------------
  Float
  ---------------------------------------------------------------------------------------------------------------*/

#define F32_BASE 4294967296.0

#ifdef NO_LIBGMP
Float encodeFloat32(Integer* man, Int iexp){
  Double r = man->value;
  if (r != 0.0){
    r = ldexp(r, iexp);
  }
  return r;
}
#else
Float encodeFloat32(Integer* man, Int iexp){
  Double r;
  Double base = F32_BASE;
  Int i;

  Int size = abs(man->size);

  /* Convert bignum to a double */
  for(r = 0.0, i = abs(size)-1; i >= 0; i--)
    r = r * base + man->data[i];
  /* Load new exponent */
  if (r != 0.0)   /* bug in mips ldexp  */
    r = ldexp(r, iexp);

  return (Float)((man->size < 0) ? -r : r);
}
#endif

/* Decode a floating point number into a mantissa an an exponent.
   This operation is tricky to do in a portable and efficient way!
   Current implementation is really bad!
*/

#define F32_MINEXP (FLT_MIN_EXP - FLT_MANT_DIG - 1)
#define F32_HIGHBIT 0x00800000
#define F32_MSBBIT  0x80000000

IntegerNode* decodeFloat32(Float32 f, Int* rExp){
  /* Do some bit fiddling on IEEE */
  Int32 high;
  Int iexp, sign;

  union { Float f; UInt32 i32; } u;

  u.f = f;
  high = u.i32;

  if ((high & ~F32_MSBBIT) == 0) {
    *rExp = 0;
    return (IntegerNode*)make_integer(0);
  }
  iexp = ((high >> 23) & 0xff) + F32_MINEXP;
  sign = high;
  high &= F32_HIGHBIT-1;
  if (iexp != F32_MINEXP) /* don't add hidden bit to denorms */
    high |= F32_HIGHBIT;
  else {
    iexp++;
    /* A denorm, normalize the mantissa */
    while (! (high & F32_HIGHBIT)) {
      high <<= 1;
      iexp--;
    }
  }
  *rExp = iexp;
  return (IntegerNode*)make_integer((sign < 0) ? -high : high);
}

/*---------------------------------------------------------------------------------------------------------------
  Double
  ---------------------------------------------------------------------------------------------------------------*/

#define F64_BASE 4294967296.0

#ifdef NO_LIBGMP
Double encodeFloat64(Integer* man, Int iexp){
   Double r = man->value;
   if (r != 0.0){
     r = ldexp(r, iexp);
   }
   return r;
}
#else
Double encodeFloat64(Integer* man, Int iexp){
  Double       r;
  Double base = F64_BASE;
  Int i;
  Int size = abs(man->size);

  /* Convert bignum to a double */
  for(r = 0.0, i = size-1; i >= 0; i--)
    r = r * base + man->data[i];

  if (r != 0.0)     /* bug in mips ldexp    */
    r = ldexp(r, iexp);

  return (man->size < 0) ? -r : r;
}
#endif

/*
 * Decode a double-precision floating point number into an Integer mantissa
 * and an Int exponent.
 */

#define F64_MINEXP (DBL_MIN_EXP - DBL_MANT_DIG - 1)
#define F64_HIGHBIT 0x00100000
#define F64_MSBBIT  0x80000000

IntegerNode* decodeFloat64(Float64 f, Int* rExp){
  UInt32      low, high;
  Int         iexp;
  Int         sign;
  Int         size;

  union { Double d; UInt32 i32[2]; UInt64 i64; } u;

  u.d = f;

#if IS_BIG_ENDIAN
  low = u.i32[1]; high = u.i32[0];
#else
  low = u.i32[0]; high = u.i32[1];
#endif

  /* Do some bit fiddling on IEEE double-precision */
  if (low == 0 && (high & ~F64_MSBBIT) == 0) {
    *rExp = 0;
    return (IntegerNode*)make_integer(0);
  }
  size = wordsof(Double);
  iexp = ((high >> 20) & 0x7ff) + F64_MINEXP;
  sign = high;
  high &= F64_HIGHBIT-1;
  if (iexp != F64_MINEXP) /* don't add hidden bit to denorms */
    high |= F64_HIGHBIT;
  else {
    iexp++;
    /* A denorm, normalize the mantissa */
    while (! (high & F64_HIGHBIT)) {
      high <<= 1;
      if (low & F64_MSBBIT)
        high++;
      low <<= 1;
      iexp--;
    }
  }
  *rExp = iexp;
  return (IntegerNode*)make_integer_32((sign < 0) ? -size : size, low, high);
}


