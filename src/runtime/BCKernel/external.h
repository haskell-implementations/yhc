#ifndef __external_h__
#define __external_h__

#include "module.h"

#define EXT_PRIMITIVE_MODULE "YHC/Primitive"

void*     ext_load(Module* mod, Char* name);
void      ext_finish(Module* mod);


#endif
