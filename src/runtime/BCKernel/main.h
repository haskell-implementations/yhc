#ifndef __main_h__
#define __main_h__

#define DEFAULT_HEAP_SIZE       16*1024*1024
#define DEFAULT_MAX_STACK_SIZE  1*1024*1024

typedef struct _Options {
  UInt        heapSize;
  UInt        maxStackSize;
  Bool        stats;
  Bool        pretty;
}Options;


extern int       G_argc;
extern char**    G_argv;
extern char*     G_progName;
extern Char*     G_yhcBasePath;
extern Options   G_options;


#endif
