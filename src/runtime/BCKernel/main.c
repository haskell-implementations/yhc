#include "platform.h"
#include "module.h"
#include "main.h"
#include "primitive.h"
#include "pretty.h"
#include "heap.h"
#include "integer.h"
#include "sanity.h"
#include "iofuncs.h"
#include "profile.h"
#include "thread.h"
#include "hsffi.h"
#include "basepath.h"

/* global variable storing the program name and arguments */
int          G_argc;
char**       G_argv;
char*        G_progName;
Options      G_options;
Char*        G_yhcBasePath;

extern Int64 G_insCount;

#ifndef VERSION
# define VERSION "Unversioned"
#endif

void version() {
  fprintf(stderr, "yhi: The York Haskell Interpreter version %s\n", VERSION);
}

/* give usage information and exit */
void usage(){
  version();
#ifdef HAT
  fprintf(stderr, "\t\tCompiled with Hat support\n");
#endif
  fprintf(stderr, "\nusage: %s [options] classfile [args]\n", G_progName);
  fprintf(stderr, "\n");
  fprintf(stderr, "  classfile            - name of haskell class file to execute\n");
  fprintf(stderr, "  args                 - arguments to pass to haskell program\n");
  fprintf(stderr, "\n");
  fprintf(stderr, "options:\n");
  fprintf(stderr, "  -h --heap size       - set the heap size in bytes, e.g -h 10M\n");
  fprintf(stderr, "  -s --stack size      - set the maximum stack size in bytes, e.g. -s 1M\n");
  fprintf(stderr, "  -stats               - print statistics on the execution\n");
  fprintf(stderr, "  -v --version         - print the Yhi version then exit\n");
  fprintf(stderr, "  -dump                - don't execute bytecode, just print it and exit\n");
  fprintf(stderr, "\n");

  exit(1);
}

/* parse a size argument */
Int parseSize(const char* arg, const char* p){
  char* end;

  Int ret = (Int)strtoul(p, &end, 10);
  Int mult = 0;

  if (end == p){
    fprintf(stderr, "ERROR: expected size argument after '%s'\n", arg);
    usage();
  }
  switch (*end++){
  case 'b': mult = 1; break;
  case 'K': mult = 1024; break;
  case 'M': mult = 1024*1024; break;
  case 'G': mult = 1024*1024*1024; break;
  default:
    fprintf(stderr, "ERROR: unknown size argument '%s' after '%s'\n", p, arg);
    fprintf(stderr, "       expected:  digit*(b|K|M|G)\n");
    fprintf(stderr, "           e.g. 3000b, 10K, 12M, 1G\n");
    usage();
  }
  if (*end){
    fprintf(stderr, "ERROR: unexpected '%c' after size argument '%s'\n", *end, p);
    usage();
  }
  return ret*mult;
}

/* parse all the arguments */
char* parseArgs(int argc, char** argv){
  char* mainMod = NULL;
  int i;

  /* initialize options */
  G_progName = argv[0];
  G_options.heapSize = DEFAULT_HEAP_SIZE;
  G_options.maxStackSize = DEFAULT_MAX_STACK_SIZE;
  G_options.stats = false;
  G_options.pretty = false;

  /* parse arguments */
  for (i = 1; i < argc; i++){
    char* arg = argv[i];
    char* next = (i+1 < argc) ? argv[i+1] : NULL;
    if (*arg != '-'){
      mainMod = arg;
      i++;
      break;
    }
    if (!strcmp(arg, "-h") || !strcmp(arg, "--heap")){
      if (!next){
        fprintf(stderr, "ERROR: expected heap size after option %s\n", arg);
        usage();
      }
      G_options.heapSize = parseSize(arg, next);
      i++;
    }else if (!strcmp(arg, "-s") || !strcmp(arg, "--stack")){
      if (!next){
        fprintf(stderr, "ERROR: expected stack size after option %s\n", arg);
        usage();
      }
      G_options.maxStackSize = parseSize(arg, next);
      i++;
    }else if (!strcmp(arg, "-stats")){
      G_options.stats = true;
    }else if (!strcmp(arg, "-dump")){
      G_options.pretty = true;
    }else if (!strcmp(arg, "-v") || !strcmp(arg, "-version") || !strcmp(arg, "--version")){
      version();
      exit(0);
    }else{
      fprintf(stderr, "WARNING: ignored unknown flag '%s'\n", arg);
    }
  }
  /* check we have a main module */
  if (!mainMod){
    usage();
  }
  /* store argument information */
  G_argc = argc - i;
  G_argv = &argv[i];

  /* get yhc base path */
  G_yhcBasePath = basepath_get();
  return mainMod;
}

/* called when the main program exits */
static void main_exit(){
    if (!G_options.stats){
        return;
    }
    printf("---- Yhc Stats ----------------------------------\n");
    printf(" num GCs:        %ld\n", G_gcStats.numGCs);
    printf(" words used:     %Lu\n", G_gcStats.wordsUsed);
    printf(" words moved:    %Lu\n", G_gcStats.wordsMoved);
    printf(" max live size:  %ld\n", G_gcStats.maxLive);
    printf("-------------------------------------------------\n");
}

/* called on program exit */
void exitHandler(void){
  main_exit();
#ifdef HAT
  hgm_exit();
#endif
  heap_exit();
  PROFILE_END();
}

/* initalize globals */
void initGlobals(char* mainMod, Node** mainFunc, Node** _toplevel, FInfo** _driver){
  Module* prelude;
  Object* mobj;
  int i;

  prelude = mod_load("Prelude");
  G_nodeUnit = mod_resolve("Prelude", "()")->node;
  G_nodeTrue = mod_resolve("Prelude", "True")->node;
  G_nodeFalse = mod_resolve("Prelude", "False")->node;

  prim_load(prelude);

  mobj = mod_resolveDirect(mainMod, "main");
  if (!mobj){
    Char* buffer = (Char*)malloc(strlen(mainMod)+strlen(".hbc")+1);
    sprintf(buffer,"%s.hbc", mainMod);
    mobj = mod_resolveDirect(buffer, "main");
    free(buffer);
    if (!mobj){
       fprintf(stderr, "ERROR: couldn't open file '%s'\n", mainMod);
       exit(1);
    }
  }
  *mainFunc  = mobj->node;
  *_toplevel = mod_resolve("YHC/_Driver", "_toplevel")->node;
  *_driver   = (FInfo*)mod_resolve("YHC/_Driver", "_driver")->info;

  G_nodeNil = mod_resolve("Prelude", "[]")->node;
  G_infoCons = (CInfo*)mod_resolve("Prelude", ":")->info;
  G_infoRight = (CInfo*)mod_resolve("Prelude", "Right")->info;
  G_nodeNothing = mod_resolve("Prelude", "Nothing")->node;
  G_infoJust = (CInfo*)mod_resolve("Prelude", "Just")->info;

  G_nodeZapArg = mod_resolve("Prelude", "_zap_arg")->node;
  G_nodeZapStack = mod_resolve("Prelude", "_zap_stack")->node;
  G_nodeBlackHole = mod_resolve("Prelude", "_black_hole")->node;
  G_infoBlackHole = (FInfo*)mod_resolve("Prelude", "_black_hole")->info;
  G_infoEBox = (CInfo*)mod_resolve("YHC/Primitive", "_E")->info;

  /* tuples */
  for (i = 1; i < MAX_TUPLE_SIZE; i++){
    char buff[MAX_TUPLE_SIZE+4];

    if (i == 1){
        G_infoTuple[i] = (CInfo*)mod_resolve("Prelude","1()")->info;
    }else{
        int j;
        buff[0] = '(';
        for (j = 0; j < (i-1); j++){
            buff[j+1] = ',';
        }
        buff[j+1] = ')';
        buff[j+2] = '\0';
        G_infoTuple[i] = (CInfo*)mod_resolve("Prelude", buff)->info;
    }
  }
}

/* initialize program */
void init(char* mainMod, Node** mainFunc, Node** _toplevel, FInfo** _driver){
  /* inits */
  sanity_init();
  heap_init(G_options.heapSize);
#ifdef HAT
  hgm_init(mainMod);
#endif
  mod_init();

  /* load all globals */
  initGlobals(mainMod, mainFunc, _toplevel, _driver);

  /* initialize the threads system */
  yhi_thread_init();
  hsffi_init();

  /* finished with the module system now */
  /* mod_exit(); ... not any more, now we still need it! */
}

/* pretty all symbols */
static void pretty_all(){
    int i;
    for (i = 0; i < G_modules->size; i++){
        HashLink* p;
        for (p = G_modules->table[i]; p != NULL; p = p->next){
            Module* mod = (Module*)p->value;
            pr_module(mod);
        }
    }
}

/* program start point */
int main(int argc, char** argv){
  Node* _toplevel;
  Node* mainFunc;
  Node* mainAp;
  FInfo* _driver;
  //Module* prelude;
  char* mainMod;

  PROFILE_BEGIN();
  atexit(exitHandler);

  /* parse program arguments */
  mainMod = parseArgs(argc, argv);

  /* initialize */
  init(mainMod, &mainFunc, &_toplevel, &_driver);

  /* pretty the modules if that was specified */
  if (G_options.pretty){
    pretty_all();
    exit(0);
  }

  /* create the main application */
  mainAp = (Node*)heap_alloc(wordsof(Node) + 2);
  MAKE_NODE(mainAp, &_driver->papTable[2], N_NORMAL);
  mainAp->args[0] = _toplevel;
  mainAp->args[1] = mainFunc;

  /* turn on gc, because we're ready to go now */
  G_gcEnabled = true;

  /* run the program */
  run(mainAp);
  return 0;
}
