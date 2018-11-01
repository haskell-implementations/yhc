#include "../hsffi.h"
#include "../main.h"
#include "../iofuncs.h"

Int primSystem(Char* cmd){
  return (Int)system(cmd);
}

Char* primGetProgName(void){
  return G_progName;
}

Char* primGetEnv(Char* env){
  return getenv(env);
}

void* primGetArg(Int i){
  if (i < 0 || i >= G_argc){
    return NULL;
  }else{
    return G_argv[i];
  }
}

Node* primExitWith(Int i){
  exit(i);
}

Int primCreateDir(Char* path){
  return mkdir(path, 0);
}

Int primRemoveDir(Char* path){
  return (Int)rmdir(path);
}

Int primRenameDir(Char* from, Char* to){
  return (Int)rename(from, to);
}

Int primRenameFile(Char* from, Char* to){
  return (Int)rename(from, to);
}

Char* primGetDirName(struct dirent* dir){
  return dir->d_name;
}

Int primCloseDir(DIR* dir){
  return (Int)closedir(dir);
}

void* primOpenDir(const char* name){
  return opendir(name);
}

Char* primGetCurrentDir(){
  Int size = 256;
  while (true){
    Char* buf = (Char*)malloc(sizeof(Char) * size);
    Char* ret = getcwd(buf, size);
    if (!ret && errno == ERANGE){
      size *= 2;
      buf = (Char*)realloc(buf, sizeof(Char) * size);
    }else{
      return ret;
    }
  }
}

void* primNextDir(void* dir){
    return readdir((DIR*)dir);
}

Int primSetCurrentDir(Char* dir){
  return (Int)chdir(dir);
}

Char* primGetHomeDir(){
  /* FIXME: won't work on windows */
  return getenv("HOME");
}

Int primRemoveFile(Char* path){
  return (Int)remove(path);
}

Int primCopyFile(Char* src, Char* dst){
  FILE* sp = fopen(src, "rb");
  FILE* dp = fopen(dst, "wb");
  if (!sp || !dp){
    return -1;
  }
  while (true){
    UByte buff[4096];
    Int read, wrote;

    read = fread(buff, 1, sizeof(buff), sp);
    wrote = fwrite(buff, 1, read, dp);
    if (ferror(sp) || ferror(dp)){
      fclose(sp);
      fclose(dp);
      return -1;
    }
    if (feof(sp)){
      fclose(sp);
      fclose(dp);
      return 0;
    }
  }
}

Bool primDirExists(Char* path){
  struct stat st;
  if (stat(path, &st) != 0) return false;
# ifdef WIN32
    return st.st_mode & _S_IFREG;
# else
    return S_ISDIR(st.st_mode);
# endif
}

Bool primFileExists(Char* path){
    return file_exists(path);
}

/* FIXME: permissions, modification times, strange paths ... */

void primSystem_init(){
  prim_addFun("primSystem", primSystem);
  prim_addFun("primGetProgName", primGetProgName);
  prim_addFun("primGetEnv", primGetEnv);
  prim_addFun("primGetArg", primGetArg);
  prim_addFun("primExitWith", primExitWith);

  prim_addFun("primCreateDir", primCreateDir);
  prim_addFun("primRemoveDir", primRemoveDir);
  prim_addFun("primRenameDir", primRenameDir);
  prim_addFun("primOpenDir", primOpenDir);
  prim_addFun("primGetDirName", primGetDirName);
  prim_addFun("primNextDir", primNextDir);
  prim_addFun("primCloseDir", primCloseDir);
  prim_addFun("primGetCurrentDir", primGetCurrentDir);
  prim_addFun("primSetCurrentDir", primSetCurrentDir);
  prim_addFun("primGetHomeDir", primGetHomeDir);
  prim_addFun("primRemoveFile", primRemoveFile);
  prim_addFun("primRenameFile", primRenameFile);
  prim_addFun("primCopyFile", primCopyFile);
  prim_addFun("primFileExists", primFileExists);
  prim_addFun("primDirExists", primDirExists);
}
