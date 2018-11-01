#include "platform.h"
#include "external.h"
#include "pretty.h"
#include "primitive.h"
#include "heap.h"
#include "main.h"

#include "hsffi.h"

#define ASSERT(x) assert(x)

/*char* strdup(const char*);*/

void* dll_open(Char* name);
void* dll_sym(void* handle, Char* name);
Char* dll_error();

/* makes a path relative if it isn't already */
static Char* path_relative(Char* path, Char* buff, Int size){
  Char* p;
  for (p = path; *p; p++){
    if (*p == '/' || *p == '\\'){
      strncpy(buff, path, size);
      return buff;
    }
  }
  snprintf(buff, size, "./%s", path);
  return buff;
}

/* load an external symbol from a dynamically linked module */
void* ext_load(Module* mod, Char* name){
  void* obj;
  Char* err;

  /* special case for primitives */
  if (!strcmp(mod->name, EXT_PRIMITIVE_MODULE)){
    obj = prim_lookupFun(name);
    if (!obj){
      fprintf(stderr, "ERROR: there is no primitive function '%s'\n", name);
      exit(-1);
    }
    return obj;
  }
  /* otherwise the normal case */
  if (!mod->external){
    Char buff[256];

    path_relative(mod->name, buff, sizeof(buff));
    mod->external = dll_open(buff);

    if (!mod->external){
      fprintf(stderr, "ERROR: couldn't load external module '%s', demanded from module '%s'\n", buff, mod->name);
      fprintf(stderr, "\t%s\n", dll_error());
      exit(-1);
    }
  }
  obj = dll_sym(mod->external, name);
  err = dll_error();
  if (err){
    fprintf(stderr, "ERROR: couldn't load function '%s' from '%s.so'\n", name, mod->path);
    fprintf(stderr, "\t%s\n", err);
    exit(-1);
  }
  return obj;
}

/*----------------------------------------------------------------------------------------------------*/
/* defined for unix like platforms */
#if defined(NO_SHARED)

void* dll_open(Char* name){
    fprintf(stderr, "ERROR: this version of yhc was compiled without shared library support!\n");
}

void* dll_sym(void* handle, Char* name){
    dll_open("");
}

Char* dll_error(){
    dll_open("");
}

#elif defined(UNIX)

#include <dlfcn.h>

void* dll_open(Char* name){
  char buff[256];
  snprintf(buff, sizeof(buff), "%s.so", name);
  return dlopen(buff, RTLD_LAZY);
}

void* dll_sym(void* handle, Char* name){
  return dlsym(handle, name);
}

Char* dll_error(){
  return dlerror();
}

#elif defined(WIN32)

#define WIN32_MEAN_AND_LEAN
#include <windows.h>

void* dll_open(Char* name){
  int i;
  char buff[256];

  for (i = 0; name[i] != 0; i++){
    if (name[i] == '/'){
      name[i] = '\\';
    }
  }
  snprintf(buff, sizeof(buff), "%s.dll", name);
  return LoadLibrary(buff);
}

void* dll_sym(void* handle, Char* name){
  return GetProcAddress((HINSTANCE) handle, name);
}

Char* dll_error(){
  return "";
}

#else
# error "No code for loading dynamic link libraries has been written for this platform ..."
#endif

