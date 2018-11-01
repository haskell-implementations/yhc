#include "platform.h"
#include "module.h"
#include "iofuncs.h"
#include "heap.h"
#include "external.h"
#include "primitive.h"
#include "hsffi.h"
#include "main.h"
#include "basepath.h"

#define DEBUG 0

#if DEBUG
# define SHOW(x) x
#else
# define SHOW(x)
#endif

/*char* strdup(const char*);*/

/* the global module table */
Hashtable* G_modules = NULL;

/* the global string table */
static Hashtable* G_strings = NULL;

/* the number of bytes allocated in loading the modules */
UInt G_staticAlloced = 0;

/* prototypes */
Char* mod_getString(Module* mod, UInt idx);
void mod_loadObject(Module* mod, UInt offs, Object* obj);
Char* mod_loadId(Module* mod, Char* sep);
void mod_free(Char* name, Module* mod);

/* initialise the module system */
void mod_init(){
  G_modules = hash_init();
  G_strings = hash_init();
}

/* a hash free function that just frees the key */
static void str_free(Char* key, void* value){
  free(key);
}

/* exit the module system, frees all the hash tables */
void mod_exit(){
  hash_free(G_modules, (HashFreeFunc)mod_free);
  G_modules = NULL;
  hash_free(G_strings, str_free);
  G_strings = NULL;
}

/* link in another module into the module table */
void mod_linkModule(Module* mod){
  hash_add(G_modules, mod->name, mod);
}

/* open a module file of a particular name, returns a pointer to the path found
(no extension) */
FILE* mod_openFile(Char* modName, Char** path){
  Char buff[256];
  Char baseBuff[512];

  Char* base = G_yhcBasePath;
  if (base == NULL){
    fprintf(stderr, "ERROR: couldn't neither find environment variable YHC_BASE_PATH nor find %s in the PATH!\n", G_progName);
    exit(1);
  }
  sprintf(buff, "%s.hbc", modName);

  SHOW(printf("--- module '%s'\n", modName));
  if (file_exists(buff)){
    *path = strdup(modName);
    return fopen(buff, "rb");
  }
  sprintf(baseBuff, "%s/lib/yhc/packages/yhc-base/1.0/%s.hbc", base, modName);
  if (file_exists(baseBuff)){
    sprintf(buff, "%s/lib/yhc/packages/yhc-base/1.0/%s", base, modName);
    *path = strdup(buff);
    return fopen(baseBuff, "rb");
  }
  fprintf(stderr, "ERROR: couldn't find module named '%s'\n", modName);
  fprintf(stderr, "tried:\n");
  fprintf(stderr, "\t%s\n", buff);
  fprintf(stderr, "\t%s\n", baseBuff);
  exit(1);
}

/* ask whether a module is loaded */
Bool mod_isLoaded(Char* modName){
  return hash_lookup(G_modules, modName) != NULL;
}

/* get a loaded module */
Module* mod_getModule(Char* modName){
  return hash_lookup(G_modules, modName);
}

/* generic version of module loading */
Module* mod_loadGeneric(FILE* fp, Char* modName, Char* path){
  Module* ret;
  char magic[5];
  UInt16 major;
  UInt16 minor;
  UInt16 flags;
  UInt i;

  assert(fp != NULL);
  ret = (Module*)malloc(sizeof(Module));
  ret->file = fp;
  ret->path = path;
  ret->external = NULL;

  /* load the module header */
  fread(magic, 1, 4, fp);
  magic[4] = '\0';
  if (strcmp(magic, "HSBC")){
    fprintf(stderr, "ERROR: module '%s' is not haskell byte code!\n", modName);
    exit(1);
  }
  major = fGetUInt16(fp);
  minor = fGetUInt16(fp);
  if (major != VERSION_MAJOR || minor != VERSION_MINOR){
    fprintf(stderr, "ERROR: module '%s' is version %d.%d, should be %d.%d\n", modName, major, minor,
            VERSION_MAJOR, VERSION_MINOR);
    exit(1);
  }
  flags = fGetUInt16(fp); /* flags */
  ret->numObjects = fGetUInt16(fp);
  ret->numStrings = fGetUInt16(fp);

  /* read the string table */
  ret->strings = (StringRef*)malloc(sizeof(StringRef) * ret->numStrings);
  for (i = 0; i < ret->numStrings; i++){
    UInt16 size;

    ret->strings[i].string = NULL;
    ret->strings[i].offset = ftell(fp);
    size = fGetUInt16(fp);

    /* skip the string data */
    fseek(fp, size, SEEK_CUR);
  }

  /* read the module name */
  ret->name = mod_loadId(ret, "/");
  hash_add(G_modules, ret->name, ret);

# ifdef HAT
  ret->hatModule = (HModule*)malloc(sizeof(HModule));
  ret->hatModule->isTrusted = !(flags & HMFL_TRACED);
  ret->hatModule->tracePtr = 0;
  ret->hatModule->name = strdup(ret->name); /* need to duplicate because ret->name will get freed */
  ret->hatModule->parent = NULL;
# endif

  /* read the objects */
  ret->lookup = hash_init();
  ret->objects = (ObjectRef*)malloc(sizeof(ObjectRef) * ret->numObjects);
  for (i = 0; i < ret->numObjects; i++){
    UInt16 size;
    Char* sname;

    ret->objects[i].object = NULL;
    ret->objects[i].offset = ftell(fp);

    sname = mod_loadId(ret, ";");
    size = fGetUInt16(fp);

    /* skip over data */
    fseek(fp, size, SEEK_CUR);

    /* add to the lookup table */
    hash_add(ret->lookup, sname, &ret->objects[i]);
  }
  return ret;
}

/* load a module directly from a file */
Module* mod_loadDirect(Char* filename){
    FILE* fp;

    if (file_exists(filename)){
        fp = fopen(filename, "rb");
        if (!fp){
            return NULL;
        }
        return mod_loadGeneric(fp, filename, filename);
    }else{
        return NULL;
    }
}

/* load a module with a particular name */
Module* mod_load(Char* modName){
  Char* path;
  FILE* fp;
  Module* ret;

  assert(modName != NULL);
  /* maybe it's loaded already */
  ret = hash_lookup(G_modules, modName);
  if (ret != NULL){
    return ret;
  }
  /* open the file and initialise */
  fp = mod_openFile(modName, &path);
  return mod_loadGeneric(fp, modName, path);
}

/* get a string from the module table of a module */
Char* mod_getString(Module* mod, UInt idx){
  Char* str;
  Char* found;
  UInt posn;

  assert(mod != NULL && idx >= 0 && idx < mod->numStrings);

  /* check whether it's loaded already */
  str = mod->strings[idx].string;
  if (str != NULL){
    return str;
  }
  /* otherwise jump to the right position in the file and load it */
  posn = ftell(mod->file);
  fseek(mod->file, mod->strings[idx].offset, SEEK_SET);
  str = fGetString(mod->file);
  fseek(mod->file, posn, SEEK_SET);

  /* add an entry into the string table if needed */
  found = hash_lookup(G_strings, str);
  if (found == NULL){
    hash_add(G_strings, str, str);
    found = str;
  }else{
    free(str);
  }
  mod->strings[idx].string = found;
  return found;
}

/* load a HPos from a file */
void mod_loadHPos(Module* mod, HPos* pos){
#ifdef HAT
  pos->tracePtr = 0;
  pos->module   = mod->hatModule;
  pos->start    = (UInt)fGetInt32(mod->file);
  pos->end      = (UInt)fGetInt32(mod->file);
#endif
}

/* generic version of resolve that works with a module and an item */
static Object* mod_resolveGeneric(Module* mod, Char* item){
  ObjectRef* ref = hash_lookup(mod->lookup, item);
  if (!ref){
    fprintf(stderr, "Couldn't find '%s' in module '%s'\n", item, mod->name);
    abort();
    exit(1);
  }
  if (ref->object == NULL){
    ref->object = (Object*)malloc(sizeof(Object));
    mod_loadObject(mod, ref->offset, ref->object);
  }
  return ref->object;
}

/* specialized version of resolve to open a particular module filename */
Object* mod_resolveDirect(Char* filename, Char* item){
    Module* mod = mod_loadDirect(filename);
    if (!mod){
        return NULL;
    }
    return mod_resolveGeneric(mod, item);
}

/* resolve an object of a particular name in a module of a particular name */
Object* mod_resolve(Char* module, Char* item){
    Module* mod = mod_load(module);
    return mod_resolveGeneric(mod, item);
}

/* load an id from a module, seperated by a particular seperator */
Char* mod_loadId(Module* mod, Char* sep){
  UInt8 size;
  Char** parts;
  UInt i, len = 0;
  Char* p;
  UInt sepLen = strlen(sep);

  /* read the size and allocate a string for the different parts */
  size = fGetUInt8(mod->file);
  parts = (Char**)malloc(sizeof(Char*) * size);
  for (i = 0; i < size; i++){
    UInt16 idx = fGetUInt16(mod->file);
    parts[i] = mod_getString(mod, idx);
    len += strlen(parts[i]) + sepLen;
  }

  /* allocate the array for the combined string */
  p = (Char*)malloc(sizeof(Char) * len);
  G_staticAlloced += sizeof(Char) * len;

  /* join the strings together */
  *p = '\0';
  for (i = 0; i < size; i++){
    strcat(p, parts[i]);
    if (i != size-1){
      strcat(p, sep);
    }
  }
  free(parts);
  return p;
}

/* split a module name A.B.C.D into A.B.C and D*/
Char* mod_splitId(Char* id){
  Int size = strlen(id);
  Int i;
  for (i = size; i >= 0; i--){
    if (id[i] == '/'){
      id[i] = '\0';
      return &id[i+1];
    }
  }
  return NULL;
}

/* make a constant node in the heap */
Node* mod_constNode(CInfo* info, Node* arg){
  Node* ret = (Node*)heap_alloc(wordsof(Node) + info->size);
  MAKE_NODE(ret, info, N_NORMAL);
  INIT_HATNODE(ret, NULL /* FIXME: should store position in file really ... */);
  ret->args[0] = arg;
  return ret;
}

/* load a constable table from a module */
void mod_loadConstTable(Module* mod, FInfo* finfo){
  UInt i;
  finfo->numConsts = fGetUInt16(mod->file);
  if (!finfo->numConsts){
    finfo->constTypes = NULL;
    finfo->constTable = NULL;
    return;
  }
  finfo->constTypes = (UByte*)malloc(sizeof(UByte) * finfo->numConsts);
  finfo->constTable = (ConstItem*)malloc(sizeof(ConstItem) * finfo->numConsts);

  G_staticAlloced += sizeof(UByte) * finfo->numConsts + sizeof(ConstItem) * finfo->numConsts;

  for (i = 0; i < finfo->numConsts; i++){
    UInt8 type = fGetUInt8(mod->file);
    Object* ref;
    Char* modId;
    Char* itemId;

    switch (type){
    case 'i':
      finfo->constTypes[i] = C_NODE;
      finfo->constTable[i] = (ConstItem)mod_constNode(&G_infoInt, (Node*)(Word)fGetInt32(mod->file));
      break;
    case 's':
      finfo->constTypes[i] = C_NODE;
      finfo->constTable[i] = (ConstItem)mod_constNode(&G_infoString, (Node*)fGetString(mod->file));
      break;
    case 'd':{
        DoubleNode* dnode = (DoubleNode*)mod_constNode(&G_infoDouble, NULL);
        dnode->value = fGetDouble(mod->file);
        finfo->constTypes[i] = C_NODE;
        finfo->constTable[i] = (ConstItem)dnode;
      }
      break;
    case 'f':{
        FloatNode* fnode = (FloatNode*)mod_constNode(&G_infoFloat, NULL);
        fnode->value = fGetFloat(mod->file);
        finfo->constTypes[i] = C_NODE;
        finfo->constTable[i] = (ConstItem)fnode;
      }
      break;
    case 'l':{
      IntegerNode* in = fGetInteger(mod->file);
      finfo->constTypes[i] = C_NODE;
      finfo->constTable[i] = (ConstItem)in;
      break;
    }
#ifdef HAT
    case 'p':{
      /* position, used in debugging, allocate hpos */
      HPos* pos = (HPos*)malloc(sizeof(HPos));
      mod_loadHPos(mod, pos);
      /* store into constant table */
      finfo->constTypes[i] = C_POS;
      finfo->constTable[i] = (ConstItem)pos;
      break;
    }
    case 'v':{
      /* load variable description */
      HVarDesc* desc = (HVarDesc*)malloc(sizeof(HVarDesc));
      desc->name = fGetString(mod->file);
      mod_loadHPos(mod, &desc->pos);
      /* store in constant table */
      finfo->constTypes[i] = C_VAR_DESC;
      finfo->constTable[i] = (ConstItem)desc;
      break;
    }
#endif
    case 'A': case '0': case 'Z': case 'F': case 'C': case 'P':
      modId = mod_loadId(mod, "/");
      itemId = mod_loadId(mod, ";");
      if (type == 'P'){
        ref = mod_resolve("_Builtin", itemId);
      }else{
        SHOW(printf("resolving const table item %d[%s;%s], in %s;%s\n",
                    (unsigned)i, modId, itemId, mod->name, finfo->name));
        ref = mod_resolve(modId, itemId);
      }
      switch(type){
      case 'A': case '0': case 'Z':
        finfo->constTypes[i] = C_NODE;
        finfo->constTable[i] = (ConstItem)ref->node;
        break;
      default:
        finfo->constTypes[i] = C_INFO;
        finfo->constTable[i] = (ConstItem)ref->info;
        break;
      }
      break;
    default:
      fprintf(stderr, "Module %s is corrupt! Const table entry %u is '%c'[0x%02d] in function %s\n",
              mod->name, (unsigned)i, type, type, finfo->name);
      exit(1);
    }
  }
}

/* swap bytes round */
UInt16 mod_swap(UInt8* code){
#if !IS_BIG_ENDIAN
  UInt8 tmp = code[0];
  code[0] = code[1];
  code[1] = tmp;
#endif
  return *(UInt16*)code;
}

/* fix a table_switch instruction */
Int mod_fixTable(UInt8* code){
  Int i;
  UInt16 size = mod_swap(&code[1]);
  UInt16* table = (UInt16*)&code[3];
  for (i = 0; i < size; i++){
    mod_swap((UInt8*)&table[i]);
  }
  return 1 + 2 + size*2;
}

/* fix a lookup_switch instruction */
Int mod_fixLookup(UInt8* code){
  Int i;
  UInt16 size = mod_swap(&code[1]);
  UInt16* table = (UInt16*)&code[5];

  mod_swap(&code[3]);
  for (i = 0; i < size; i++){
    mod_swap((UInt8*)&table[i*2]);
    mod_swap((UInt8*)&table[i*2+1]);
  }
  return 1 + 4 + size*4;
}

/* fix an instruction */
Int mod_fixIns(Module* mod, Char* name, UInt8* code){
  if (*code == END_CODE){
    return 0;
  }
  switch(*code){
#  define op(n,x) case x: return 1;
#  define op1(n,x) case x: return 2;
#  define op2(n,x) case x: mod_swap(&code[1]); return 3;
#  define op1S(n,x) case x: return 2;
#  define op2S(n,x) case x: mod_swap(&code[1]); return 3;
#  define op1_1(n,x) case x: return 3;
#  define op2_1(n,x) case x: mod_swap(&code[1]); return 4;
#  define opT(n,x) case x: return mod_fixTable(code);
#  define opL(n,x) case x: return mod_fixLookup(code);
#  define opJ(n,x) case x: mod_swap(&code[1]); return 3;
#    include "bytecodes.h"
#  undef op
#  undef op1
#  undef op2
#  undef op1S
#  undef op2S
#  undef op1_1
#  undef op2_1
#  undef opT
#  undef opL
#  undef opJ
  default:
    fprintf(stderr, "module '%s' is corrupt! unrecognised instruction '0x%x' in function %s\n",
            mod->name, *code, name);
    exit(1);
  }
}

/* fix the code in a module, swaps byte orders around */
void mod_fixCode(Module* mod, Char* name, UInt8* code){
  UInt8* p = code;

  for (;;){
    Int z = mod_fixIns(mod, name, p);
    if (!z){
      break;
    }
    p += z;
  }
}

/* load a function object from a module */
void mod_loadFun(Module* mod, Char* name, Object* obj){
  FInfo* finfo;
  UInt8 arity;
  UInt size, stack, flags;


  arity = fGetUInt8(mod->file);
  stack = fGetUInt16(mod->file);
  flags = fGetUInt8(mod->file);

  finfo = finfo_alloc(arity, stack, flags, name, 0, NULL, 0, NULL, NULL, obj, mod, NULL /* FIXME: position */);

  /* now we can load the const table ... */

  mod_loadConstTable(mod, finfo);

  size = fGetUInt16(mod->file);
  finfo->codeSize = size;
  finfo->code = (UInt8*)malloc(sizeof(UInt8) * size);
  G_staticAlloced += sizeof(UInt8) * size;

  fread(finfo->code, sizeof(UInt8), size, mod->file);

  mod_fixCode(mod, name, finfo->code);

  /* Check if this is a selector, and if so mark it */
  if (finfo->code[SELECTOR_INS] == SELECTOR_EVAL){
    /* then this is a selector, so mark it as such */
    finfo->flags |= FFL_SELECTOR;
  }

  SHOW(printf("**** FUN %p:'%s.%s', arity %d, consts %d, code-size %u\n",
              finfo, mod->name, finfo->name, arity, finfo->numConsts, (unsigned)size));

#if 0
  pr_finfo(finfo);
#endif
}

/* load a con from a module */
void mod_loadCon(Module* mod, Char* name, Object* obj){
  CInfo* cinfo;
  Int size = fGetUInt8(mod->file);
  Int num = fGetUInt8(mod->file);

  cinfo = cinfo_alloc(size, name, num, CI_NONE, obj, mod, NULL /* FIXME: position */);
  SHOW(printf("**** CON '%s.%s', size %d, tag %d\n", mod->name, cinfo->name, cinfo->size, cinfo->number));
}

/* load a primitive from a module */
void mod_loadPrim(Module* mod, Char* name, Object* obj){
  /* FIXME: do some clever stuff ... */
  Object* _builtin = mod_resolve("_Builtin", name);

  SHOW(printf("**** PRIM '%s' as '_Builtin.%s'\n", name, ((XInfo*)_builtin->info)->name));

  obj->info = _builtin->info;
  obj->node = _builtin->node;
  if (obj->node){
    obj->global.global = &obj->node;
    heap_pushGlobal(&obj->global);
  }
}

/* load an external */
void mod_loadExternal(Module* mod, Char* name, Object* obj){
  Char* cName = fGetString(mod->file);
  UInt16 arity = fGetUInt16(mod->file);
  Char cConv = fGetUInt8(mod->file);
  Char retType = fGetUInt8(mod->file);
  Char argTypes[256];
  Int i;

  for (i = 0; i < arity; i++){
    argTypes[i] = fGetUInt8(mod->file);
  }
  if (*cName == '\0'){
    cName = strdup(name);
  }
  if (cConv == 'b'){
    // builtin is a little bit different
    Object* builtin = mod_resolve("_Builtin", name);

    obj->info = builtin->info;
    obj->node = builtin->node;
    if (obj->node){
        obj->global.global = &obj->node;
        heap_pushGlobal(&obj->global);
    }
  }else{
    // standard ffi call
    FFIFunction* ffiFunc = hsffi_loadExternal(mod, cName, arity, cConv, retType, argTypes, NULL);
    xinfo_alloc(arity, 0, name, ffiFunc, obj, mod, NULL);
  }

#if 0
  sprintf(buff, "Wrap_%s", cName);

  SHOW(printf("**** EXTERNAL '%s.%s' as '%s', arity %d\n", mod->name, name, cName, arity));
  handle = ext_load(mod, buff);

  xinfo_alloc(arity, 0, name, handle, obj, mod, NULL /* FIXME: position */);
#endif
}

/* load an object from the module */
void mod_loadObject(Module* mod, UInt offs, Object* obj){
  Char* name;
  UInt16 size;
  UInt8 type;
  UInt pos;

  /* remember where we were before we move */
  pos = ftell(mod->file);
  fseek(mod->file, offs, SEEK_SET);

  name = mod_loadId(mod, ";");
  assert(name != NULL);
  size = fGetUInt16(mod->file);
  type = fGetUInt8(mod->file);
  if (type == 'F'){
    mod_loadFun(mod, name, obj);
  }else if (type == 'C'){
    mod_loadCon(mod, name, obj);
  }else if (type == 'P'){
    mod_loadPrim(mod, name, obj);
  }else if (type == 'X'){
    mod_loadExternal(mod, name, obj);
  }else{
    fprintf(stderr, "Module is corrupted! unrecognised root object type (%c|%02x), expected {'F','C','P','X'} \n", type,type);
    exit(1);
  }
  /* make sure we go back to where we were */
  fseek(mod->file, pos, SEEK_SET);
}

/* free a module */
void mod_free(Char* name, Module* mod){
  Int i;
  free(mod->name);
  if (mod->path != NULL){
    free(mod->path);
  }
  if (mod->strings != NULL){
    free(mod->strings);
  }
  for (i = 0; i < (Int)mod->numObjects; i++){
    if (mod->objects[i].object){
      free(mod->objects[i].object);
    }
  }
  free(mod->objects);
  hash_free(mod->lookup, str_free);
  free(mod);
}
