/** the IO system, this is fairly complex because it has to work with different
    objects held underneath the handle */
#include "../hsffi.h"
#include "../integer.h"

typedef struct _HandleFuncs HandleFuncs;

/** the various buffering modes */
enum { NO_BUFFERING = -1,                /* no buffering, flush every character */
       LINE_BUFFERING = -2,              /* line buffering, flush every line */
       DEFAULT_BUFFERING = -3 };         /* whatever buffering we like */

/** possible error codes */
typedef enum {
  ERR_NONE,                     /* no error */
  ERR_UNSUPPORTED,              /* operation not supported */
  ERR_OTHER                     /* some C error, see errno */
} HandleError;

/** a handle to some IO object
    @field name       The name of the stream (might be file name for example)
    @field type       The type of the field ("FILE" for a file for example)
    @field hfuns      The stream dependent handle functions
    @field buffering  The quantity of buffering, a positive number or one of the above constants
    @field buffCount  The number of characters written since the last flush
    @field error      A string associated with the last error or null if no error
*/
typedef struct _Handle {
  char*         name;
  char*         type;
  HandleFuncs*  hfuns;
  int           buffering;
  int           buffCount;
  HandleError   error;
}Handle;

/** all the functions associated with a handle */
struct _HandleFuncs {
  void   (*putChar)(Handle* handle, char c);
  Int  (*getChar)(Handle* handle);
  Bool (*isEOF)(Handle* handle);
  void   (*seek)(Handle* handle, Int mod, Int posn);
  Int  (*tell)(Handle* handle);
  void   (*flush)(Handle* handle);
  Int  (*fileSize)(Handle* handle);
  void   (*close)(Handle* handle);
  void   (*setBuffering)(Handle* handle, Int mode);
};

static Bool checkSupported(Handle* h, void* p, char* desc){
  if (p){
    return true;
  }
  h->error = ERR_UNSUPPORTED;
  return false;
}

char* hGetFileNameC(Handle* h){
  return h->name;
}

char* hGetTypeC(Handle* h){
  return h->type;
}

Int hGetErrorC(Handle* h){
  Int ret = h->error;
  h->error = ERR_NONE;
  return ret;
}

void hPutCharC(Handle* h,char c){
  if (checkSupported(h, h->hfuns->putChar, "hPutChar")){
    h->hfuns->putChar(h, c);
  }
}

Int hGetCharC(Handle* h){
  if (checkSupported(h, h->hfuns->getChar, "hGetChar")){
    return h->hfuns->getChar(h);
  }
  return 0;
}

Bool hIsEOFC(Handle* h){
  if (checkSupported(h, h->hfuns->isEOF, "hIsEOF")){
    return h->hfuns->isEOF(h);
  }
  return 0;
}

void hSetBufferingC(Handle* h,int buff){
  if (checkSupported(h, h->hfuns->setBuffering, "hSetBuffering")){
    h->hfuns->setBuffering(h, buff);
  }
}

void hSeekC(Handle* h,Int mode,Int loc){
  if (checkSupported(h, h->hfuns->seek, "hSeek")){
   h->hfuns->seek(h, mode, loc);
  }
}

Int hTellC(Handle* h){
  if (checkSupported(h, h->hfuns->tell, "hTell")){
    return h->hfuns->tell(h);
  }
  return 0;
}

Int hGetBufferingC(Handle* h){
  return h->buffering;
}

void hFlushC(Handle* h){
  if (checkSupported(h, h->hfuns->flush, "hFlush")){
    h->hfuns->flush(h);
  }
}

Int hFileSizeC(Handle* h){
  if (checkSupported(h, h->hfuns->fileSize, "hFileSize")){
    return h->hfuns->fileSize(h);
  }
  return 0;
}

void* hCloseC(Handle* h){
  return h->hfuns->close;
}

Bool primEqHandleC(void* x ,void* y){
  return x == y;
}

/*---------------------------------------------------------------------------------------------------------
  Files
  ---------------------------------------------------------------------------------------------------------*/

/** a handle representing a file
    @field handle the base handle for this file object.
    @field file   the C file pointer */
typedef struct _File {
  Handle      handle;
  FILE*       file;
}File;

static void fCheckErrors(File* f){
  if (ferror(f->file)){
    f->handle.error = ERR_OTHER;
  }
}

void fPutChar(Handle* handle, char c){
  File* f = (File*)handle;
  fputc(c, f->file);
  fCheckErrors(f);
}

Int fGetChar(Handle* handle){
  File* f = (File*)handle;
  Int ret = fgetc(f->file);
  fCheckErrors(f);
  return ret;
}

Bool fIsEOF(Handle* handle){
  File* f = (File*)handle;
  Bool ret = feof(f->file);
  fCheckErrors(f);
  return ret;
}

void fSeek(Handle* handle, Int mode, Int posn){
  File* f = (File*)handle;
  int smode;
  switch (mode){
  case 0: smode = SEEK_SET; break;
  case 1: smode = SEEK_CUR; break;
  case 2: smode = SEEK_END; break;
  default:
    fprintf(stderr, "hSeekC with invalid mode constant %ld!\n", mode);
    exit(-1);
  }
  fseek(f->file, posn, smode);
  fCheckErrors(f);
}

Int fTell(Handle* handle){
  File* f = (File*)handle;
  Int ret = ftell(f->file);
  fCheckErrors(f);
  return ret;
}

void fFlush(Handle* handle){
  File* f = (File*)handle;
  fflush(f->file);
  fCheckErrors(f);
}

Int fFileSize(Handle* handle){
  File* f = (File*)handle;
  long here = ftell(f->file);
  Int end;
  fseek(f->file, 0, SEEK_END);
  end = ftell(f->file);
  fseek(f->file, here, SEEK_SET);
  fCheckErrors(f);
  return end;
}

void fClose(Handle* handle){
  File* f = (File*)handle;
  fclose(f->file);
  fCheckErrors(f);
}

void fSetBuffering(Handle* handle, Int buffer){
  handle->buffering = buffer;
}

/** the standard collection of functions for a file */
static HandleFuncs G_fileFuncs = {
  fPutChar, fGetChar, fIsEOF, fSeek, fTell, fFlush, fFileSize, fClose, fSetBuffering
};

/** create a new file handle */
static Handle* fNewHandle(char* filename, FILE* fp, int buff){
  File* f = (File*)malloc(sizeof(File));

  f->handle.name      = filename;
  f->handle.type      = "FILE";
  f->handle.hfuns     = &G_fileFuncs;
  f->handle.buffering = buff;
  f->handle.buffCount = 0;
  f->handle.error     = ERR_NONE;
  f->file = fp;
  return (Handle*)f;
}


/* standard handles */
void* stdinC(void){ return fNewHandle("<stdin>", stdin, LINE_BUFFERING); }
void* stdoutC(void){ return fNewHandle("<stdout>", stdout, LINE_BUFFERING); }
void* stderrC(void){ return fNewHandle("<stderr>", stderr, LINE_BUFFERING); }

void* openFileC(char* name,Int m){
  char* mode = NULL;
  FILE* fp;

  switch (m){
  case 0: mode = "r"; break;
  case 1: mode = "w"; break;
  case 2: mode = "a"; break;
  case 3: mode = "r+"; break;
  }
  fp = fopen(name, mode);
  if (!fp){
    return NULL;
  }
  return fNewHandle(name, fp, DEFAULT_BUFFERING);
}

void primIO_init(){
  prim_addFun("stdinC", stdinC);
  prim_addFun("stderrC", stderrC);
  prim_addFun("stdoutC", stdoutC);
  prim_addFun("hGetFileNameC", hGetFileNameC);
  prim_addFun("hGetTypeC", hGetTypeC);
  prim_addFun("hGetErrorC", hGetErrorC);
  prim_addFun("openFileC", openFileC);
  prim_addFun("hIsEOFC", hIsEOFC);
  prim_addFun("hSetBufferingC", hSetBufferingC);
  prim_addFun("hGetBufferingC", hGetBufferingC);
  prim_addFun("hSeekC", hSeekC);
  prim_addFun("hTellC", hTellC);
  prim_addFun("hPutCharC", hPutCharC);
  prim_addFun("hGetCharC", hGetCharC);
  prim_addFun("hFlushC", hFlushC);
  prim_addFun("hFileSizeC", hFileSizeC);
  prim_addFun("hCloseC", hCloseC);
  prim_addFun("primEqHandleC", primEqHandleC);
}
