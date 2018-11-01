#include "../process.h"

/*-------------------------------------------------------------------------------------------------
  Exceptions
  -------------------------------------------------------------------------------------------------*/

void primBlockExceptions(){
    proc_excepBlock();
}

void primUnblockExceptions(){
    proc_excepUnblock();
}

void primException_init(){
    prim_addFun("primBlockExceptions", primBlockExceptions);
    prim_addFun("primUnblockExceptions", primUnblockExceptions);
}


