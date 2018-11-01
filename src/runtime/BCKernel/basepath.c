/* this is here because guessing the YHC_BASE_PATH is somewhat involved:

     - if argv[0] was qualified then we should try relative to that

     - otherwise we should search the PATH environment variable
       this is separated differently on different operating systems
*/

#include "platform.h"
#include "iofuncs.h"
#include "main.h"

#ifdef WIN32
# define SEPARATOR ';'
#else
# define SEPARATOR ':'
#endif

/* allocating sprintf */
Char* alloc_sprintf(Char* fmt, ...){
  va_list va;
  Int size = 16;
  Char* buff = (Char*)malloc(size);

  while (true){
    Int need;

    va_start(va, fmt);
    need = vsnprintf(buff, size, fmt, va);
    va_end(va);
    if (need > -1 && need < size){
      return buff;
    }
    if (need > -1){
      size = need+1;
    }else{
      size *= 2;
    }
    buff = (Char*)realloc(buff, size);
  }
}

/* return NULL if you don't find it */ 
static Char* basepath_check(Char* directory, Char* suffix){
  Char* search;
  Bool res;
  
  search = alloc_sprintf("%s/%s%s", directory, G_progName, suffix);
  res = file_exists(search);
  
  free(search);
  return (!res ? NULL : alloc_sprintf("%s/..", directory));
}

/* guess the basepath */
static Char* basepath_guess(){
  Char buff[512];
  Char* progBase;
  Char* path;
  Char* p;
  Char* last;
  Char* res;
  int n;

  /* check whether argv[0] included some kind of path */
  progBase = file_basename(G_progName, buff, sizeof(buff));
  if (strcmp(progBase, ".")){
    return alloc_sprintf("%s/..", progBase);
  }

  /* otherwise search the PATH environment variable for G_progName ... */
  last = getenv("PATH");
  n = strlen(last);
  path = malloc((n + 2) * sizeof(Char));
  memcpy(path, last, n * sizeof(Char));
  path[n] = SEPARATOR;
  path[n+1] = 0;
  last = path;
  
  for (p = path; *p; p++){
    if (*p != SEPARATOR){
      continue;
    }
    *p = '\0';
    
    /* search for progname in that directory */
    res = basepath_check(last, "");
    if (res) goto finish;
    
#ifdef WIN32
    res = basepath_check(last, ".exe");
    if (res) goto finish;
#endif
    
    last = p+1;
  }
  /* not found! */
  res = NULL;

finish:  
  free(path);
  return res;
}

/* get the yhc base path */
Char* basepath_get(){
  /* try the environment var first ... */
  Char* env = getenv("YHC_BASE_PATH");
  if (env){
    return strdup(env);
  }
  /* otherwise guess ... */
  return basepath_guess();
}


