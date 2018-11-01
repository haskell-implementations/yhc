#include "iofuncs.h"
#include "heap.h"
#include "integer.h"

UInt8 fGetUInt8(FILE* fp){
  return (UInt8)fgetc(fp);
}

Int8 fGetInt8(FILE* fp){
  return (Int8)fGetUInt8(fp);
}

UInt16 fGetUInt16(FILE* fp){
  UInt hi = fGetUInt8(fp);
  UInt lo = fGetUInt8(fp);
  return (UInt16)(hi << 8 | lo);
}

Int16 fGetInt16(FILE* fp){
  return (Int16)fGetUInt16(fp);
}

UInt32 fGetUInt32(FILE* fp){
  UInt32 hh = fGetUInt8(fp);
  UInt32 hl = fGetUInt8(fp);
  UInt32 lh = fGetUInt8(fp);
  UInt32 ll = fGetUInt8(fp);
  return hh << 24 | hl << 16 | lh << 8 | ll;
}

Int32 fGetInt32(FILE* fp){
  return (Int32)fGetUInt32(fp);
}

Char* fGetString(FILE* fp){
  UInt16 len = fGetUInt16(fp);
  Char* ret = (Char*)malloc(len+1);
  fread(ret, 1, len, fp);
  ret[len] = '\0';
  return ret;
}

Float fGetFloat(FILE* fp){
  mpz_t mant;
  Int exp;
  Integer* imant;
  Float ret;

  fGetMPZ(fp, mant);
  exp = fGetInt16(fp);

  imant = (Integer*)malloc(sizeof(Integer) + getMPZAlloc(mant)*sizeof(Word));
  _fromGMPInteger(mant, imant);
  ret = encodeFloat32(imant, exp);
  free(imant);
  return ret;
}

Double fGetDouble(FILE* fp){
  mpz_t mant;
  Int exp;
  Integer* imant;
  Double ret;

  fGetMPZ(fp, mant);
  exp = fGetInt16(fp);

  imant = (Integer*)malloc(sizeof(Integer) * getMPZAlloc(mant)*sizeof(Word));
  _fromGMPInteger(mant, imant);
  ret = encodeFloat64(imant, exp);
  free(imant);
  return ret;
}

void fGetMPZ(FILE* fp, mpz_t ret){
  mpz_t p;

  Int8 len = fGetInt8(fp);
  Int plen = ((len < 0) ? -len : len);
  Int i;

  mpz_init(ret);
  mpz_init(p);

  mpz_set_ui(ret, 0);
  for (i = plen-1; i >= 0; i--){
    UInt8 c = fGetUInt8(fp);

    mpz_ui_pow_ui(p, 256, (unsigned)i);
    mpz_mul_ui(p, p, c);
    mpz_add(ret, ret, p);
  }
  if (len < 0){
    mpz_neg(ret,ret);
  }
  mpz_clear(p);
}

IntegerNode* fGetInteger(FILE* fp){
  IntegerNode* ret;
  mpz_t tmp;

  fGetMPZ(fp, tmp);
  ret = fromGMPInteger(tmp);
  mpz_clear(tmp);
  return ret;
}

/* special hack for windows */
#ifdef WIN32
#define S_ISREG(s) ((s) & _S_IFREG)
#endif

Bool file_exists(Char* path){
  struct stat st;
  if (stat(path, &st) != 0) return false;
  return S_ISREG(st.st_mode);
}


/* strip the file extension from a file */
Char* file_stripExt(Char* path){
  Char* p = &path[strlen(path)];
  for (p = &path[strlen(path)]; p != path; p--){
    if (*p == '.'){
      *p = '\0';
      return path;
    }else if (*p == '\\' || *p == '/'){
      return path;
    }
  }
  return path;
}


/* get the basename of a path i.e. /usr/bin/yhc becomes /usr/bin */
Char* file_basename(Char* path, Char* buff, Int size){
  Char* p = &path[strlen(path)];
  for (; p != path; p--){
    if (*p == '\\' || *p == '/'){
      Int take = MIN(size, p-path);
      memset(buff, 0, size);
      strncpy(buff, path, take);
      return buff;
    }
  }
  strncpy(buff, ".", size);
  return buff;
}
