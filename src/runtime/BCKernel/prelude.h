#ifndef __prelude_h__
#define __prelude_h__

Node* Wrap_stdinC(Node* node);
Node* Wrap_stderrC(Node* node);
Node* Wrap_stdoutC(Node* node);
Node* Wrap_hGetFileNameC(Node* node);
Node* Wrap_openFileC(Node* node);
Node* Wrap_hIsEOF(Node* node);
Node* Wrap_hSetPosnC(Node* node);
Node* Wrap_hSetBufferingC(Node* node);
Node* Wrap_hSeekC(Node* node);
Node* Wrap_hPutChar(Node* node);
Node* Wrap_hGetPosnC(Node* node);
Node* Wrap_hGetChar(Node* node);
Node* Wrap_hFlushC(Node* node);
Node* Wrap_primHFileSizeC(Node* node);
Node* Wrap_hCloseC(Node* node);
Node* Wrap_primEqHandlePosnC(Node* node);
Node* Wrap_primEqHandleC(Node* node);

Node* Wrap_primSystem(Node* node);
Node* Wrap_primGetProgName(Node* node);
Node* Wrap_primGetEnv(Node* node);
Node* Wrap_ptrToString(Node* node);
Node* Wrap_cGetArg(Node* node);
Node* Wrap_ptrToCString(Node* node);
Node* Wrap_primExitWith(Node* node);

Node* Wrap_intToAddr(Node* node);
Node* Wrap_addrToInt(Node* node);

Node* Wrap_getErrNo(Node* node);

Node* Wrap_foreignObjToAddr(Node* node);
Node* Wrap_primForeignObjC(Node* node);

Node* Wrap__free(Node* node);
Node* Wrap__realloc(Node* node);
Node* Wrap__malloc(Node* node);

Node* Wrap_ptrToInt(Node* node);
Node* Wrap_intToPtr(Node* node);
Node* Wrap_castPtr(Node* node);

Node* Wrap_writeInt64AtAddr(Node* node);
Node* Wrap_readInt64AtAddr(Node* node);
Node* Wrap_writeInt32AtAddr(Node* node);
Node* Wrap_readInt32AtAddr(Node* node);
Node* Wrap_writeInt16AtAddr(Node* node);
Node* Wrap_readInt16AtAddr(Node* node);
Node* Wrap_writeInt8AtAddr(Node* node);
Node* Wrap_readInt8AtAddr(Node* node);
Node* Wrap_writeWord64AtAddr(Node* node);
Node* Wrap_readWord64AtAddr(Node* node);
Node* Wrap_writeWord32AtAddr(Node* node);
Node* Wrap_readWord32AtAddr(Node* node);
Node* Wrap_writeWord16AtAddr(Node* node);
Node* Wrap_readWord16AtAddr(Node* node);
Node* Wrap_writeWord8AtAddr(Node* node);
Node* Wrap_readWord8AtAddr(Node* node);
Node* Wrap_writeDoubleAtAddr(Node* node);
Node* Wrap_readDoubleAtAddr(Node* node);
Node* Wrap_writeFloatAtAddr(Node* node);
Node* Wrap_readFloatAtAddr(Node* node);
Node* Wrap_writeAddrAtAddr(Node* node);
Node* Wrap_readAddrAtAddr(Node* node);
Node* Wrap_writeIntAtAddr(Node* node);
Node* Wrap_readIntAtAddr(Node* node);
Node* Wrap_writeCharAtAddr(Node* node);
Node* Wrap_readCharAtAddr(Node* node);

#endif
