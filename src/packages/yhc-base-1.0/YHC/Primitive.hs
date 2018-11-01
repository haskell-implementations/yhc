module YHC.Primitive where
#ifndef __HADDOCK__

-------------------------------------------------------------------------------------------------------
-- Data.Array
-------------------------------------------------------------------------------------------------------

data Vector a; -- internal to compiler

foreign import primitive primCopyVectorC :: Vector a -> IO (Vector a)
foreign import primitive primNewVectorC :: Int -> _E a -> IO (Vector a)
foreign import primitive primVectorIndexC :: Vector a -> Int -> a
foreign import primitive primUpdateVectorC :: Int -> _E a -> Vector a -> IO ()

-------------------------------------------------------------------------------------------------------
-- Data.IORef
-------------------------------------------------------------------------------------------------------

data IORef a; -- internal to compiler

foreign import primitive primNewIORefC :: a -> IO (IORef a)
foreign import primitive primReadIORefC :: IORef a -> IO a
foreign import primitive primWriteIORefC :: IORef a -> a -> IO ()

-------------------------------------------------------------------------------------------------------
-- Data.PackedString
-------------------------------------------------------------------------------------------------------

data PackedString;

foreign import primitive primPackString :: Int -> String -> PackedString
foreign import fastccall primPSGetChar :: PackedString -> Int -> Char
foreign import fastccall primComparePS :: PackedString -> PackedString -> Int

-------------------------------------------------------------------------------------------------------
-- Prelude
-------------------------------------------------------------------------------------------------------

foreign import primitive primIntAbs :: Int -> Int
foreign import primitive primIntSignum :: Int -> Int
foreign import primitive primIntFromInteger :: Integer -> Int
foreign import primitive primIntegerFromInt :: Int -> Integer
foreign import primitive primFloatExp :: Float -> Float
foreign import primitive primFloatLog :: Float -> Float
foreign import primitive primFloatSqrt :: Float -> Float
foreign import primitive primFloatSin :: Float -> Float
foreign import primitive primFloatCos :: Float -> Float
foreign import primitive primFloatTan :: Float -> Float
foreign import primitive primFloatASin :: Float -> Float
foreign import primitive primFloatACos :: Float -> Float
foreign import primitive primFloatATan :: Float -> Float
foreign import primitive primFloatPow :: Float -> Float -> Float
foreign import primitive primFloatFromInteger :: Integer -> Float
foreign import primitive primFloatAbs :: Float -> Float
foreign import primitive primFloatSignum :: Float -> Float
foreign import primitive primDecodeFloat :: Float -> (Integer,Int)
foreign import primitive primEncodeFloat :: Integer -> Int -> Float
foreign import primitive primDoubleExp :: Double -> Double
foreign import primitive primDoubleLog :: Double -> Double
foreign import primitive primDoubleSqrt :: Double -> Double
foreign import primitive primDoubleSin :: Double -> Double
foreign import primitive primDoubleCos :: Double -> Double
foreign import primitive primDoubleTan :: Double -> Double
foreign import primitive primDoubleASin :: Double -> Double
foreign import primitive primDoubleACos :: Double -> Double
foreign import primitive primDoubleATan :: Double -> Double
foreign import primitive primDoublePow :: Double -> Double -> Double
foreign import primitive primDoubleFromInteger :: Integer -> Double
foreign import primitive primDoubleAbs :: Double -> Double
foreign import primitive primDoubleSignum :: Double -> Double
foreign import primitive primDecodeDouble :: Double -> (Integer,Int)
foreign import primitive primEncodeDouble :: Integer -> Int -> Double
foreign import primitive primIntegerEq :: Integer -> Integer -> Bool
foreign import primitive primIntegerNe :: Integer -> Integer -> Bool
foreign import primitive primIntegerLt :: Integer -> Integer -> Bool
foreign import primitive primIntegerLe :: Integer -> Integer -> Bool
foreign import primitive primIntegerGe :: Integer -> Integer -> Bool
foreign import primitive primIntegerGt :: Integer -> Integer -> Bool
foreign import primitive primIntegerAdd :: Integer -> Integer -> Integer
foreign import primitive primIntegerSub :: Integer -> Integer -> Integer
foreign import primitive primIntegerMul :: Integer -> Integer -> Integer
foreign import primitive primIntegerNeg :: Integer -> Integer
foreign import primitive primIntegerQuotRem :: Integer -> Integer -> (Integer, Integer)
foreign import primitive primIntegerQuot :: Integer -> Integer -> Integer
foreign import primitive primIntegerRem :: Integer -> Integer -> Integer
foreign import primitive primStrError :: Int -> CString
foreign import primitive primUnsafeCoerce :: a -> b

unsafeCoerce x = primUnsafeCoerce x

-------------------------------------------------------------------------------------------------------
-- System.IO
-------------------------------------------------------------------------------------------------------

newtype Handle = Handle (ForeignPtr ())

foreign import fastccall hGetErrorC :: Handle -> IO Int
foreign import fastccall primEqHandleC :: Handle -> Handle -> Bool
foreign import ccall hCloseC :: Handle -> FinalizerPtr ()
foreign import ccall hFileSizeC :: Handle -> IO Int
foreign import ccall hGetCharC :: Handle -> IO Int
foreign import ccall hFlushC :: Handle -> IO ()
foreign import fastccall hGetFileNameC :: Handle -> IO CString
foreign import fastccall hGetTypeC :: Handle -> IO CString
foreign import ccall hIsEOFC :: Handle -> IO Bool
foreign import ccall hPutCharC :: Handle -> Char -> IO ()
foreign import ccall hSeekC :: Handle -> Int -> Int -> IO ()
foreign import ccall hSetBufferingC :: Handle -> Int -> IO ()
foreign import ccall hGetBufferingC :: Handle -> IO Int
foreign import ccall hTellC :: Handle -> IO Int
foreign import ccall openFileC :: CString -> Int -> IO (Ptr ())
foreign import fastccall stdoutC :: () -> Ptr ()
foreign import fastccall stderrC :: () -> Ptr ()
foreign import fastccall stdinC :: () -> Ptr ()

-------------------------------------------------------------------------------------------------------
-- System.Exit
-------------------------------------------------------------------------------------------------------

foreign import fastccall primExitWith :: Int -> a

-------------------------------------------------------------------------------------------------------
-- System.Environment
-------------------------------------------------------------------------------------------------------

foreign import fastccall primGetProgName :: IO CString
foreign import fastccall primGetArg :: Int -> IO CString
foreign import fastccall primGetEnv :: CString -> IO CString

-------------------------------------------------------------------------------------------------------
-- System.Directory
-------------------------------------------------------------------------------------------------------

type Dir = Ptr ()
type Dirent = Ptr ()

foreign import ccall primCreateDir :: CString -> IO Int
foreign import ccall primRemoveDir :: CString -> IO Int
foreign import ccall primRenameDir :: CString -> CString -> IO Int
foreign import ccall primOpenDir :: CString -> IO Dir
foreign import ccall primGetDirName :: Dirent -> IO CString
foreign import ccall primNextDir :: Dir -> IO Dirent
foreign import ccall primCloseDir :: Dir -> IO Int
foreign import ccall primGetCurrentDir :: IO CString
foreign import ccall primSetCurrentDir :: CString -> IO Int
foreign import ccall primGetHomeDir :: IO CString
foreign import ccall primRemoveFile :: CString -> IO Int
foreign import ccall primRenameFile :: CString -> CString -> IO Int
foreign import ccall primCopyFile :: CString -> CString -> IO Int
foreign import ccall primFileExists :: CString -> IO Bool
foreign import ccall primDirExists :: CString -> IO Bool

-------------------------------------------------------------------------------------------------------
-- Control.Exception
-------------------------------------------------------------------------------------------------------

foreign import builtin primCatch :: a -> (b -> a) -> a
foreign import builtin primThrow :: a -> b
foreign import builtin primThrowTo :: ThreadId -> b -> () -> ()
foreign import fastccall primBlockExceptions :: () -> IO ()
foreign import fastccall primUnblockExceptions :: () -> IO ()

-------------------------------------------------------------------------------------------------------
-- Control.Concurrent
-------------------------------------------------------------------------------------------------------

data ThreadId;

-- box to prevent evaluation
data _E a = _E a

foreign import primitive primSpawnProcess :: _E a -> IO ThreadId
foreign import primitive primKillProcess :: IO ()
foreign import primitive primSpinLockProcess :: Int -> IO Int
foreign import primitive primMyThreadId :: () -> IO ThreadId

-------------------------------------------------------------------------------------------------------
-- Control.Concurrent.MVar
-------------------------------------------------------------------------------------------------------

data MVar a;

foreign import primitive primNewMVar :: IO (MVar a)
foreign import primitive primPutMVar :: MVar a -> a -> IO Bool
foreign import primitive primTakeMVar :: MVar a -> IO a
foreign import primitive primReadMVar :: MVar a -> IO a
foreign import primitive primSwapMVar :: MVar a -> a -> IO a
foreign import primitive primIsEmptyMVar :: MVar a -> IO Bool
foreign import primitive primTryTakeMVar :: MVar a -> IO (Maybe a)

-------------------------------------------------------------------------------------------------------
-- Foreign.Ptr
-------------------------------------------------------------------------------------------------------

data Ptr a;
data FunPtr a;

foreign import cast ptrToInt :: Ptr a -> Int
foreign import cast intToPtr :: Int -> Ptr a
foreign import cast castPtr :: Ptr a -> Ptr b
foreign import cast funPtrToInt :: FunPtr a -> Int
foreign import cast castFunPtrToPtr :: FunPtr a -> Ptr b
foreign import cast castPtrToFunPtr :: Ptr a -> FunPtr b

-------------------------------------------------------------------------------------------------------
-- Foreign.ForeignPtr
-------------------------------------------------------------------------------------------------------

data ForeignPtr a;
type FinalizerPtr a = FunPtr (Ptr a -> IO ())

foreign import primitive primCreateForeignPtr :: Ptr a -> IO (ForeignPtr a)
foreign import primitive primAttachForeignPtr :: FinalizerPtr a -> ForeignPtr a -> IO ()
foreign import primitive primDerefForeignPtr :: ForeignPtr a -> Ptr a
foreign import primitive primFreeForeignPtr :: ForeignPtr a -> IO ()

-------------------------------------------------------------------------------------------------------
-- Foreign.StablePtr
-------------------------------------------------------------------------------------------------------

newtype StablePtr a = StablePtr (Ptr ())

foreign import fastccall primCreateStablePtr :: a -> IO (StablePtr a)
foreign import fastccall primFreeStablePtr :: StablePtr a -> IO ()
foreign import fastccall primDerefStablePtr ::StablePtr a -> IO a

-------------------------------------------------------------------------------------------------------
-- Foreign.C.Error
-------------------------------------------------------------------------------------------------------

foreign import fastccall getErrorNo :: IO Int

-------------------------------------------------------------------------------------------------------
-- Foreign.C.String
-------------------------------------------------------------------------------------------------------

newtype CString = CString (Ptr Char)

foreign import primitive primNewCString :: Int -> String -> IO CString
foreign import fastccall primFreeCString :: CString -> IO ()

-------------------------------------------------------------------------------------------------------
-- Foreign.Marshal.Alloc
-------------------------------------------------------------------------------------------------------

foreign import fastccall primMalloc :: Int -> IO (Ptr a)
foreign import fastccall primFree :: Ptr a -> IO ()
foreign import fastccall primRealloc :: Ptr a -> Int -> IO (Ptr b)
foreign import fastccall primFinalizerFree :: () -> FinalizerPtr a

-------------------------------------------------------------------------------------------------------
-- Foreign.Marshal.Util
-------------------------------------------------------------------------------------------------------

foreign import fastccall primMemcpy :: Ptr a -> Ptr a -> Int -> IO ()
foreign import fastccall primMemmove :: Ptr a -> Ptr a -> Int -> IO ()

-------------------------------------------------------------------------------------------------------
-- Foreign.Int
-------------------------------------------------------------------------------------------------------

data Int8;
data Int16;
data Int32;
data Int64;

foreign import fastccall primEqInt8 :: Int8 -> Int8  -> Bool
foreign import fastccall primLtInt8 :: Int8 -> Int8  -> Bool
foreign import fastccall primLeInt8 :: Int8 -> Int8  -> Bool
foreign import fastccall primGtInt8 :: Int8 -> Int8  -> Bool
foreign import fastccall primGeInt8 :: Int8 -> Int8  -> Bool
foreign import fastccall primAddInt8 :: Int8 -> Int8  -> Int8
foreign import fastccall primSubInt8 :: Int8 -> Int8  -> Int8
foreign import fastccall primMulInt8 :: Int8 -> Int8  -> Int8
foreign import fastccall primAbsInt8 :: Int8       -> Int8
foreign import fastccall primSignumInt8 :: Int8       -> Int8
foreign import fastccall primQuotInt8 :: Int8 -> Int8  -> Int8
foreign import fastccall primRemInt8 :: Int8 -> Int8  -> Int8
foreign import fastccall primToEnumInt8 :: Int     -> Int8
foreign import fastccall primFromEnumInt8 :: Int8       -> Int
foreign import fastccall primInt8FromInteger :: Integer -> Int8
foreign import fastccall primInt8ToInteger :: Int8       -> Integer

foreign import fastccall primEqInt16 :: Int16 -> Int16  -> Bool
foreign import fastccall primLtInt16 :: Int16 -> Int16  -> Bool
foreign import fastccall primLeInt16 :: Int16 -> Int16  -> Bool
foreign import fastccall primGtInt16 :: Int16 -> Int16  -> Bool
foreign import fastccall primGeInt16 :: Int16 -> Int16  -> Bool
foreign import fastccall primAddInt16 :: Int16 -> Int16  -> Int16
foreign import fastccall primSubInt16 :: Int16 -> Int16  -> Int16
foreign import fastccall primMulInt16 :: Int16 -> Int16  -> Int16
foreign import fastccall primAbsInt16 :: Int16       -> Int16
foreign import fastccall primSignumInt16 :: Int16       -> Int16
foreign import fastccall primQuotInt16 :: Int16 -> Int16  -> Int16
foreign import fastccall primRemInt16 :: Int16 -> Int16  -> Int16
foreign import fastccall primToEnumInt16 :: Int     -> Int16
foreign import fastccall primFromEnumInt16 :: Int16       -> Int
foreign import fastccall primInt16FromInteger :: Integer -> Int16
foreign import fastccall primInt16ToInteger :: Int16       -> Integer

foreign import fastccall primEqInt32 :: Int32 -> Int32  -> Bool
foreign import fastccall primLtInt32 :: Int32 -> Int32  -> Bool
foreign import fastccall primLeInt32 :: Int32 -> Int32  -> Bool
foreign import fastccall primGtInt32 :: Int32 -> Int32  -> Bool
foreign import fastccall primGeInt32 :: Int32 -> Int32  -> Bool
foreign import fastccall primAddInt32 :: Int32 -> Int32  -> Int32
foreign import fastccall primSubInt32 :: Int32 -> Int32  -> Int32
foreign import fastccall primMulInt32 :: Int32 -> Int32  -> Int32
foreign import fastccall primAbsInt32 :: Int32       -> Int32
foreign import fastccall primSignumInt32 :: Int32       -> Int32
foreign import fastccall primQuotInt32 :: Int32 -> Int32  -> Int32
foreign import fastccall primRemInt32 :: Int32 -> Int32  -> Int32
foreign import fastccall primToEnumInt32 :: Int     -> Int32
foreign import fastccall primFromEnumInt32 :: Int32       -> Int
foreign import fastccall primInt32FromInteger :: Integer -> Int32
foreign import fastccall primInt32ToInteger :: Int32       -> Integer

foreign import fastccall primEqInt64 :: Int64 -> Int64  -> Bool
foreign import fastccall primLtInt64 :: Int64 -> Int64  -> Bool
foreign import fastccall primLeInt64 :: Int64 -> Int64  -> Bool
foreign import fastccall primGtInt64 :: Int64 -> Int64  -> Bool
foreign import fastccall primGeInt64 :: Int64 -> Int64  -> Bool
foreign import fastccall primAddInt64 :: Int64 -> Int64  -> Int64
foreign import fastccall primSubInt64 :: Int64 -> Int64  -> Int64
foreign import fastccall primMulInt64 :: Int64 -> Int64  -> Int64
foreign import fastccall primAbsInt64 :: Int64       -> Int64
foreign import fastccall primSignumInt64 :: Int64       -> Int64
foreign import fastccall primQuotInt64 :: Int64 -> Int64  -> Int64
foreign import fastccall primRemInt64 :: Int64 -> Int64  -> Int64
foreign import fastccall primToEnumInt64 :: Int     -> Int64
foreign import fastccall primFromEnumInt64 :: Int64       -> Int
foreign import fastccall primInt64FromInteger :: Integer -> Int64
foreign import fastccall primInt64ToInteger :: Int64       -> Integer

-------------------------------------------------------------------------------------------------------
-- Foreign.Word
-------------------------------------------------------------------------------------------------------

data Word8;
data Word16;
data Word32;
data Word64;

foreign import fastccall primEqWord8 :: Word8 -> Word8  -> Bool
foreign import fastccall primLtWord8 :: Word8 -> Word8  -> Bool
foreign import fastccall primLeWord8 :: Word8 -> Word8  -> Bool
foreign import fastccall primGtWord8 :: Word8 -> Word8  -> Bool
foreign import fastccall primGeWord8 :: Word8 -> Word8  -> Bool
foreign import fastccall primAddWord8 :: Word8 -> Word8  -> Word8
foreign import fastccall primSubWord8 :: Word8 -> Word8  -> Word8
foreign import fastccall primMulWord8 :: Word8 -> Word8  -> Word8
foreign import fastccall primAbsWord8 :: Word8       -> Word8
foreign import fastccall primSignumWord8 :: Word8       -> Word8
foreign import fastccall primQuotWord8 :: Word8 -> Word8  -> Word8
foreign import fastccall primRemWord8 :: Word8 -> Word8  -> Word8
foreign import fastccall primToEnumWord8 :: Int     -> Word8
foreign import fastccall primFromEnumWord8 :: Word8       -> Int
foreign import fastccall primWord8FromInteger :: Integer -> Word8
foreign import fastccall primWord8ToInteger :: Word8       -> Integer

foreign import fastccall primEqWord16 :: Word16 -> Word16  -> Bool
foreign import fastccall primLtWord16 :: Word16 -> Word16  -> Bool
foreign import fastccall primLeWord16 :: Word16 -> Word16  -> Bool
foreign import fastccall primGtWord16 :: Word16 -> Word16  -> Bool
foreign import fastccall primGeWord16 :: Word16 -> Word16  -> Bool
foreign import fastccall primAddWord16 :: Word16 -> Word16  -> Word16
foreign import fastccall primSubWord16 :: Word16 -> Word16  -> Word16
foreign import fastccall primMulWord16 :: Word16 -> Word16  -> Word16
foreign import fastccall primAbsWord16 :: Word16       -> Word16
foreign import fastccall primSignumWord16 :: Word16       -> Word16
foreign import fastccall primQuotWord16 :: Word16 -> Word16  -> Word16
foreign import fastccall primRemWord16 :: Word16 -> Word16  -> Word16
foreign import fastccall primToEnumWord16 :: Int     -> Word16
foreign import fastccall primFromEnumWord16 :: Word16       -> Int
foreign import fastccall primWord16FromInteger :: Integer -> Word16
foreign import fastccall primWord16ToInteger :: Word16       -> Integer

foreign import fastccall primEqWord32 :: Word32 -> Word32  -> Bool
foreign import fastccall primLtWord32 :: Word32 -> Word32  -> Bool
foreign import fastccall primLeWord32 :: Word32 -> Word32  -> Bool
foreign import fastccall primGtWord32 :: Word32 -> Word32  -> Bool
foreign import fastccall primGeWord32 :: Word32 -> Word32  -> Bool
foreign import fastccall primAddWord32 :: Word32 -> Word32  -> Word32
foreign import fastccall primSubWord32 :: Word32 -> Word32  -> Word32
foreign import fastccall primMulWord32 :: Word32 -> Word32  -> Word32
foreign import fastccall primAbsWord32 :: Word32       -> Word32
foreign import fastccall primSignumWord32 :: Word32       -> Word32
foreign import fastccall primQuotWord32 :: Word32 -> Word32  -> Word32
foreign import fastccall primRemWord32 :: Word32 -> Word32  -> Word32
foreign import fastccall primToEnumWord32 :: Int     -> Word32
foreign import fastccall primFromEnumWord32 :: Word32       -> Int
foreign import fastccall primWord32FromInteger :: Integer -> Word32
foreign import fastccall primWord32ToInteger :: Word32       -> Integer

foreign import fastccall primEqWord64 :: Word64 -> Word64  -> Bool
foreign import fastccall primLtWord64 :: Word64 -> Word64  -> Bool
foreign import fastccall primLeWord64 :: Word64 -> Word64  -> Bool
foreign import fastccall primGtWord64 :: Word64 -> Word64  -> Bool
foreign import fastccall primGeWord64 :: Word64 -> Word64  -> Bool
foreign import fastccall primAddWord64 :: Word64 -> Word64  -> Word64
foreign import fastccall primSubWord64 :: Word64 -> Word64  -> Word64
foreign import fastccall primMulWord64 :: Word64 -> Word64  -> Word64
foreign import fastccall primAbsWord64 :: Word64       -> Word64
foreign import fastccall primSignumWord64 :: Word64       -> Word64
foreign import fastccall primQuotWord64 :: Word64 -> Word64  -> Word64
foreign import fastccall primRemWord64 :: Word64 -> Word64  -> Word64
foreign import fastccall primToEnumWord64 :: Int     -> Word64
foreign import fastccall primFromEnumWord64 :: Word64       -> Int
foreign import fastccall primWord64FromInteger :: Integer -> Word64
foreign import fastccall primWord64ToInteger :: Word64       -> Integer

-------------------------------------------------------------------------------------------------------
-- Foreign.Storable
-------------------------------------------------------------------------------------------------------

foreign import fastccall primReadCharAtAddr :: Ptr Char -> IO Char
foreign import fastccall primWriteCharAtAddr :: Ptr Char -> Char -> IO ()

foreign import fastccall primReadIntAtAddr :: Ptr Int -> IO Int
foreign import fastccall primWriteIntAtAddr :: Ptr Int -> Int -> IO ()

foreign import fastccall primReadFloatAtAddr :: Ptr Float -> IO Float
foreign import fastccall primWriteFloatAtAddr :: Ptr Float -> Float -> IO ()

foreign import fastccall primReadDoubleAtAddr :: Ptr Double -> IO Double
foreign import fastccall primWriteDoubleAtAddr :: Ptr Double -> Double -> IO ()

foreign import fastccall primReadAddrAtAddr :: Ptr (Ptr a) -> IO (Ptr a)
foreign import fastccall primWriteAddrAtAddr :: Ptr (Ptr a) -> Ptr a -> IO ()

foreign import fastccall primReadWord8AtAddr :: Ptr Word8 -> IO Word8
foreign import fastccall primWriteWord8AtAddr :: Ptr Word8 -> Word8 -> IO ()

foreign import fastccall primReadWord16AtAddr :: Ptr Word16 -> IO Word16
foreign import fastccall primWriteWord16AtAddr :: Ptr Word16 -> Word16 -> IO ()

foreign import fastccall primReadWord32AtAddr :: Ptr Word32 -> IO Word32
foreign import fastccall primWriteWord32AtAddr :: Ptr Word32 -> Word32 -> IO ()

foreign import fastccall primReadWord64AtAddr :: Ptr Word64 -> IO Word64
foreign import fastccall primWriteWord64AtAddr :: Ptr Word64 -> Word64 -> IO ()

foreign import fastccall primReadInt8AtAddr :: Ptr Int8 -> IO Int8
foreign import fastccall primWriteInt8AtAddr :: Ptr Int8 -> Int8 -> IO ()

foreign import fastccall primReadInt16AtAddr :: Ptr Int16 -> IO Int16
foreign import fastccall primWriteInt16AtAddr :: Ptr Int16 -> Int16 -> IO ()

foreign import fastccall primReadInt32AtAddr :: Ptr Int32 -> IO Int32
foreign import fastccall primWriteInt32AtAddr :: Ptr Int32 -> Int32 -> IO ()

foreign import fastccall primReadInt64AtAddr :: Ptr Int64 -> IO Int64
foreign import fastccall primWriteInt64AtAddr :: Ptr Int64 -> Int64 -> IO ()

-------------------------------------------------------------------------------------------------------
-- YHC.Runtime.API
-------------------------------------------------------------------------------------------------------

data Node;

foreign import fastccall primAPILock :: () -> IO ()
foreign import fastccall primAPIUnlock :: () -> IO ()

foreign import cast primAPIToNode :: _E a -> IO Node
foreign import cast primAPIFromNode :: Node -> IO (_E a)

foreign import fastccall primAPINodeGetInfo :: Node -> IO (Ptr ())
foreign import fastccall primAPINodeGetArg :: Node -> Int -> IO Node
foreign import fastccall primAPINodeSetArg :: Node -> Int -> Node -> IO ()
foreign import fastccall primAPINodeIsNull :: Node -> Bool
foreign import fastccall primAPINewNode :: Ptr () -> Int -> IO Node

foreign import fastccall primAPIInfoGetType :: Ptr () -> IO Int

foreign import fastccall primAPIPInfoGetSize :: Ptr () -> IO Int
foreign import fastccall primAPIPInfoGetNeed :: Ptr () -> IO Int
foreign import fastccall primAPIPInfoGetFInfo :: Ptr () -> IO (Ptr ())

foreign import fastccall primAPIFInfoGetPInfo :: Ptr () -> Int -> IO (Ptr ())
foreign import fastccall primAPIFInfoGetArity :: Ptr () -> IO Int
foreign import fastccall primAPIFInfoGetStack :: Ptr () -> IO Int
foreign import fastccall primAPIFInfoGetModule :: Ptr () -> IO (Ptr ())
foreign import fastccall primAPIFInfoGetName :: Ptr () -> IO CString
foreign import fastccall primAPIFInfoGetCodeSize :: Ptr () -> IO Int
foreign import fastccall primAPIFInfoGetCode :: Ptr () -> IO (Ptr Char)
foreign import fastccall primAPIFInfoGetNumConsts :: Ptr () -> IO Int
foreign import fastccall primAPIFInfoGetConstType :: Ptr () -> Int -> IO Int
foreign import fastccall primAPIFInfoGetConstInfo :: Ptr () -> Int -> IO (Ptr ())
foreign import fastccall primAPIFInfoGetConstNode :: Ptr () -> Int -> IO Node

foreign import fastccall primAPICInfoGetSize :: Ptr () -> IO Int
foreign import fastccall primAPICInfoGetModule :: Ptr () -> IO (Ptr ())
foreign import fastccall primAPICInfoGetName :: Ptr () -> IO CString
foreign import fastccall primAPICInfoGetTag :: Ptr () -> IO Int

foreign import fastccall primAPINewFInfo :: Int -> Int -> Ptr () -> CString -> Int -> CString -> Int -> IO (Ptr ())
foreign import fastccall primAPIFInfoSetConstInfo :: Ptr () -> Int -> Ptr () -> IO ()
foreign import fastccall primAPIFInfoSetConstNode :: Ptr () -> Int -> Node -> IO ()

foreign import fastccall primAPINewCInfo :: Int -> Ptr () -> CString -> Int -> IO (Ptr ())

foreign import fastccall primAPIIsModuleLoaded :: CString -> IO Bool
foreign import fastccall primAPINewModule :: CString -> IO (Ptr ())
foreign import fastccall primAPIGetModule :: CString -> IO (Ptr ())
foreign import fastccall primAPIModuleGetName :: Ptr () -> IO CString
foreign import fastccall primAPIModuleLookupInfo :: Ptr () -> CString -> IO (Ptr ())
foreign import fastccall primAPIModuleLookupNode :: Ptr () -> CString -> IO Node
foreign import fastccall primAPIModuleLoad :: CString -> IO (Ptr ())

#endif

