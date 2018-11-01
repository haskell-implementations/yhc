#ifndef __module_h__
#define __module_h__

/* the module system works as follows.
   When the module is first loaded it first scans the string table and remembers
   for each string id where the string can be loaded in the file.

   It then scans each object reference, it reads the name id and loads the string for that
   name. It then stores the object name against the object offset in the lookup table and skips
   the rest of the object (using the size information for each object).

   Now when something wants to resolve an object it looks up the name in the lookup table, that gives
   it an object reference id. It then looks at the corresponding object reference, if the object is
   already loaded it gives that otherwise it jumps to the appropriate place in the bytecode file and
   loads the object, and then updates the object reference to note that it's been loaded */

#include "hashtable.h"
#include "node.h"
#include "heap.h"

/* module version numbers, should correspond with what the compiler produces */
#define VERSION_MAJOR 1
#define VERSION_MINOR 10

/* Which byte marks something as being a selector */
#define SELECTOR_INS            0

/* an object is something loaded fully from the bytecode file, with info being the info
   and node being the corresponding caf/zcon */
typedef struct _Object {
  Info*            info;
  Node*            node;
  Global           global;
}Object;

/* an object reference is a possibly not yet loaded reference into the bytecode file */
typedef struct _ObjectRef {
  Object*    object;
  UInt       offset;
}ObjectRef;

/* a string reference is a possibly not yet loaded string in the string table */
typedef struct _StringRef {
  Char*   string;
  UInt    offset;
}StringRef;

/* a module stores information necessary in loading the bytecode */
typedef struct _Module {
  FILE*         file;            /* file this module was loaded from */
  Char*         name;            /* the qualified name of this module */
  Char*         path;            /* the path this module was loaded from, excluding extension */
  UInt          numObjects;      /* the number of objects in the module */
  UInt          numStrings;      /* the number of strings in the module */

  StringRef*    strings;         /* string reference table */
  ObjectRef*    objects;         /* object reference table */

  Hashtable*    lookup;          /* lookup, maps object names to object references */

  void*         external;        /* external, non-null if this module has a dynamically linked
                                    external counterpart */
#ifdef HAT
  HModule*      hatModule;
#endif
}Module;

/* module functions */
void        mod_init();
void        mod_exit();
void        mod_linkModule(Module* module);
Object*     mod_resolve(Char* module, Char* item);
Object*     mod_resolveDirect(Char* filename, Char* item);
Module*     mod_load(Char* modName);
Bool        mod_isLoaded(Char* modName);
Module*     mod_getModule(Char* modName);

/* number of bytes statically allocated in loading the modules */
extern UInt G_staticAlloced;

/* the global module table */
extern Hashtable* G_modules;

/* helper functions to allocate infos correctly (since it's a bit fiddly) */
CInfo* cinfo_alloc(Int size, Char* name, Int number, Int flags, Object* obj, Module* mod, HPos* pos);
FInfo* finfo_alloc(Int arity, Int stack, Int flags, Char* name, Int codeSize, CodePtr code, Int numConsts,
                   UByte* constTypes, ConstItem* constTable, Object* obj, Module* mod, HPos* pos);
XInfo* xinfo_alloc(Int arity, Int stack, Char* name, struct _FFIFunction* func,
                   Object* obj, Module* mod, HPos* pos);


#endif
