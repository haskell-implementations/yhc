/* template for config.h, system dependant things discovered by the configure script */

/* the configure script finds the size of various types and whether the
   system is big or little endian */
#define SIZEOF_CHAR %(char)
#define SIZEOF_SHORT %(short)
#define SIZEOF_INT %(int)
#define SIZEOF_LONG %(long)
#define SIZEOF_LONG_LONG %(longlong)
#define SIZEOF_FLOAT %(float)
#define SIZEOF_DOUBLE %(double)
#define SIZEOF_VOIDP %(void)
#if %(bigendian)
#define WORDS_BIGENDIAN 1
#endif
#define HAVE_GCC_LABELS %(gcclabels)
#define HAVE_LIBPTHREAD %(libpthread)

/* first of all get constants for Int8, Int16, Int32, etc ... */
#if SIZEOF_CHAR == 1
#  define INT8_TYPE       char
#else
#  error "can't find type matching int8"
#endif

#if SIZEOF_SHORT == 2
#  define INT16_TYPE      short
#else
#  error "can't find type matching int16"
#endif

#if SIZEOF_LONG == 4
#  define INT32_TYPE      long
#elif SIZEOF_INT == 4
#  define INT32_TYPE      int
#else
#  error "can't find type matching int32"
#endif

#if SIZEOF_LONG == 8
#  define INT64_TYPE      long
#elif SIZEOF_INT == 8
#  define INT64_TYPE      int
#elif SIZEOF_LONG_LONG == 8
#  define INT64_TYPE      long long
#else
/* too common to warn about */
/* #  warning "can't find type matching int64" */
#  define INT64_TYPE      INT32_TYPE
#endif

#if SIZEOF_FLOAT == 4
#  define FLOAT32_TYPE    float
#else
#  error "can't find 32 bit floating point"
#endif

#if SIZEOF_DOUBLE == 8
#  define FLOAT64_TYPE   double
#else
#  warning "can't find 64 bit floating point"
#  define FLOAT64_TYPE   FLOAT32_TYPE
#endif

/* now find the correct type information for words */
#if SIZEOF_VOIDP == 2
/* decided trying to support 16 bits is too much, who'd want it anyway? */
#   error "YHC requires an architecture with at least 32 bit pointers"
#elif SIZEOF_VOIDP == 4
#   define WORD_TYPE        INT32_TYPE
#   define HALF_TYPE        INT16_TYPE
#   define QUARTER_TYPE     INT8_TYPE
#   define WORD_BYTES_SHIFT 2
#   define WORD_BITS_SHIFT  5
#elif SIZEOF_VOIDP == 8
#   define WORD_TYPE        INT64_TYPE
#   define HALF_TYPE        INT32_TYPE
#   define QUARTER_TYPE     INT16_TYPE
#   define WORD_BYTES_SHIFT 3
#   define WORD_BITS_SHIFT  6
#else
#   error "Word size is not 2, 4 or 8 bytes!"
#endif

/* do we have gcc labelled goto extension? */
#define USE_GCC_LABELS HAVE_GCC_LABELS

#if HAVE_LIBPTHREAD
# define USE_PTHREADS 1
#else
# define USE_PTHREADS 0
#endif


#ifdef WIN32
/* #define WIN32 */
#else
/* this is unix */
#define UNIX
#endif


#define VERSION "UNVERSIONED"


/* Skip LIBGMP and LIBFFI */
#define NO_LIBFFI
#define NO_LIBGMP
#define NO_SHARED

