#ifndef __iofuncs_h__
#define __iofuncs_h__

#include "types.h"
#include "integer.h"

/* functions to read quantities from a file */
UInt8 fGetUInt8(FILE* fp);
Int8 fGetInt8(FILE* fp);

UInt16 fGetUInt16(FILE* fp);
Int16 fGetInt16(FILE* fp);

UInt32 fGetUInt32(FILE* fp);
Int32 fGetInt32(FILE* fp);

Char* fGetString(FILE* fp);

Float fGetFloat(FILE* fp);
Double fGetDouble(FILE* fp);

void fGetMPZ(FILE* fp, mpz_t mpz);
IntegerNode* fGetInteger(FILE* fp);

/* old names, no longer used since confusing
UByte fGetUByte(FILE* fp);
Byte fGetByte(FILE* fp);
UShort fGetUShort(FILE* fp);
Short fGetShort(FILE* fp);
UInt fGetUInt(FILE* fp);
Int fGetInt(FILE* fp);
Char* fGetString(FILE* fp);
Float fGetFloat(FILE* fp);
Double fGetDouble(FILE* fp);
void fGetInteger(Integer i, FILE* fp);
*/

/* returns whether a given file exists or not */
Bool file_exists(Char* path);

/* strip the extension from a path */
Char* file_stripExt(Char* path);

/* get the basename of a path */
Char* file_basename(Char* path, Char* buff, Int size);

#endif
