#ifndef __platform_h__
#define __platform_h__

/*----------------------------------------------------------------------------------------
 platform specific definitions should go here
 ----------------------------------------------------------------------------------------*/

#define PLATFORM_H_VERSION 0x0100

#include <config.h>

/* #define NO_LIBFFI */
/* #define NO_LIBGMP */
/* #define NO_SHARED */

#if !defined(WIN32) /* windows doesn't need this check */
#  if CONFIG_H_VERSION != PLATFORM_H_VERSION
#    error "config.h is the wrong version. Please re-run configure"
#  endif
#endif

/* VS 2005 started being "security" concious */
#if defined(_MSC_VER) && (_MSC_VER >= 1400)
#  define _CRT_SECURE_NO_DEPRECATE
#endif

/* fixed size types, always the same size. Alias to different things on different platforms */
typedef signed INT8_TYPE    Int8;
typedef signed INT16_TYPE   Int16;
typedef signed INT32_TYPE   Int32;
typedef signed INT64_TYPE   Int64;
typedef unsigned INT8_TYPE  UInt8;
typedef unsigned INT16_TYPE UInt16;
typedef unsigned INT32_TYPE UInt32;
typedef unsigned INT64_TYPE UInt64;
typedef FLOAT32_TYPE        Float32;
typedef FLOAT64_TYPE        Float64;

/* type synonyms for basic types */
typedef INT8_TYPE    Char;        /* could be Int16 for unicode support .. */

/* native byte sizes, should always be a single byte */
typedef Int8                Byte;
typedef UInt8               UByte;

/* native int and word types, these *MUST* be the same as the size of a void* */
typedef WORD_TYPE           Int;
typedef unsigned WORD_TYPE  UInt;
typedef unsigned WORD_TYPE  Word;

/* native half int sizes, *MUST* be half the size of native int, whatever that is */
typedef HALF_TYPE           HInt;
typedef unsigned HALF_TYPE  HUInt;
typedef unsigned HALF_TYPE  HWord;

/* native quarter int sizes, *MUST* be 1/4 the size of a native int */
typedef QUARTER_TYPE            QInt;
typedef unsigned QUARTER_TYPE   QUInt;
typedef unsigned QUARTER_TYPE   QWord;

/* native double int size, typically twice the size of a normal int

REMOVED: it's confusing to talk about 'double ints' when they aren't on 64bit platforms
typedef Int64               DInt;
typedef UInt64              DUInt;
typedef UInt64              DWord;
*/

/* native floating point types */
typedef float              Float;
typedef double             Double;

/* endianness */
#ifdef WORDS_BIGENDIAN
# define IS_BIG_ENDIAN     1
#else
# define IS_BIG_ENDIAN     0
#endif

/* how many bytes there are in a native word */
#define WORD_BYTES       (1<<WORD_BYTES_SHIFT)
#define WORD_BYTES_MASK  (WORD_BYTES-1)

/* how many bits there are in a native word */
#define WORD_BITS        (1<<WORD_BITS_SHIFT)
#define WORD_BITS_MASK   (WORD_BITS-1)

/* how many bits there are in a byte */
#define BYTE_BITS_SHIFT  3
#define BYTE_BITS        (1<<BYTE_BITS_SHIFT)

/* standard headers */
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <math.h>
#ifndef NO_LIBGMP
#   include <gmp.h>
#endif
#include <float.h>
#include <sys/stat.h>
#include <sys/types.h>
#if defined (WIN32)
#  include "win32/dirent.h"
#else
#  include <dirent.h>
#  include <unistd.h>
#endif
#ifndef NO_LIBFFI
#  include <ffi.h>
#endif
#include <errno.h>
#include <time.h>


#if defined (WIN32)
#  define snprintf		_snprintf
#  define vsnprintf		_vsnprintf
#  define getcwd		_getcwd
#  define strdup        _strdup
#endif



#endif
