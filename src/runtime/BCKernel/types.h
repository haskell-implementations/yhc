/* defines primitive types as well as platform dependent issues */
#ifndef __types_h__
#define __types_h__

#include "platform.h"

/* boolean values */
typedef Int            Bool;

enum { false = 0, true = 1 };

/* convenient macros */
#define countof(x)     (sizeof(x)/sizeof(*x))
#define wordsof(x) (sizeof(x)/sizeof(Word))

#define MIN(x,y)       (((x)<(y))?(x):(y))
#define MAX(x,y)       (((x)>(y))?(x):(y))
#define ABS(x)         (((x)<0)?-(x):(x))

#endif
