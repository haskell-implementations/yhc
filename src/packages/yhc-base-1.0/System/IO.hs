{-# OPTIONS_YHC --unifyhack #-}
module System.IO
    (
     Handle
    ,HandlePosn
    ,IOMode(ReadMode,WriteMode,AppendMode,ReadWriteMode)
    ,BufferMode(NoBuffering,LineBuffering,BlockBuffering)
    ,SeekMode(AbsoluteSeek,RelativeSeek,SeekFromEnd)
    ,stdin, stdout, stderr, hClose
    ,openFile, hFileSize, hIsEOF, isEOF
    ,hSetBuffering, hGetBuffering
    ,hFlush, hGetPosn, hSetPosn, hSeek
    ,hWaitForInput, hReady, hGetLine, hLookAhead
    ,hGetChar
    ,hGetContents, hPutChar, hPutStr, hPutStrLn, hPrint
    ,hIsOpen, hIsClosed, hIsReadable, hIsWritable, hIsSeekable
    ,isAlreadyExistsError, isDoesNotExistError
    ,isAlreadyInUseError, isFullError, isEOFError
    ,isIllegalOperation, isPermissionError, isUserError
    ,ioeGetErrorString, ioeGetHandle, ioeGetFileName
    ,try, bracket , bracket_

    -- re-export PreludeIO things
    ,IO, FilePath, IOError(..), ioError, userError, catch{-, interact
    ,putChar, putStr, putStrLn, print, getChar, getLine, getContents
    ,readFile, writeFile, appendFile, readIO, readLn-}

    ,hGetFileName           -- not standard Haskell'98
--    ,SocketType(..), openSocket     -- not standard Haskell'98
    ,fixIO
    )where

import YHC.ErrNo
import YHC.Internal
import YHC.Primitive
import System.IO.Unsafe
import Data.Ix
import Data.IORef
import Foreign
import Foreign.C

-- The following implementation is direct from the Library Report.
bracket               :: IO a -> (a->IO b) -> (a->IO c) -> IO c
bracket before after m     = do
    x <- before
    rs <- try (m x)
    after x
    case rs of
      Right r -> return r
      Left e  -> ioError e

-- The following implementation is direct from the Library Report.
bracket_              :: IO a -> (a->IO b) -> IO c -> IO c
bracket_ before after m     = do
    x <- before
    rs <- try m
    after x
    case rs of
      Right r -> return r
      Left e  -> ioError e

data BufferMode  =  NoBuffering | LineBuffering | BlockBuffering (Maybe Int)
                     deriving (Eq, Ord, Read, Show)

data HandlePosn = HandlePosn Handle Integer

data SocketType = SocketStream | SocketDatagram | SocketRaw

instance Eq Handle where
  h == j = primEqHandleC h j

instance Eq HandlePosn where
  (HandlePosn h a) == (HandlePosn j b) = h==j && a == b


data HandleError = NoHErr | UnsupportedHErr | OtherHErr
                 deriving (Eq, Enum)

instance Enum BufferMode where
    fromEnum NoBuffering               = negate 1
    fromEnum LineBuffering             = negate 2
    fromEnum (BlockBuffering Nothing)  = negate 3
    fromEnum (BlockBuffering (Just i)) = i

    toEnum i
        | i == negate 1 = NoBuffering
        | i == negate 2 = LineBuffering
        | i == negate 3 = BlockBuffering Nothing
        | otherwise     = BlockBuffering (Just i)

hClose :: Handle -> IO ()
hClose (Handle h) = finalizeForeignPtr h

wrapPrim :: Handle -> String -> IO a -> IO a
wrapPrim h name f =
    do r <- f
       e <- hGetErrorC h
       case toEnum e of
           NoHErr -> return r
           UnsupportedHErr ->
               do typ <- hGetTypeC h >>= peekCString
                  let desc = name ++ " is not supported in handles of type " ++ typ
                  throwIOError desc Nothing (Just h) 0
           OtherHErr ->
               do errno <- getErrNo
                  throwIOError name Nothing (Just h) errno

hFileSize :: Handle -> IO Integer
hFileSize h = wrapPrim h "hFileSize" (hFileSizeC h) >>= return . toInteger

hFlush :: Handle -> IO ()
hFlush h = wrapPrim h "hFlush" (hFlushC h)

--foreign import ccall "hGetBufferingC" hGetBuffering :: Handle -> IO BufferMode

--The world ensures correct sequentialisation of IO-actions,
--especially avoids sharing of input actions.
--However, the world is not passed everywhere and the following
--code relies on nhc not perform optimisations that could
--change the evaluation order.
--E.g., don't use `const' instead of the lambda abstraction of world.

hGetChar              :: Handle -> IO Char
hGetChar h =
    do c <- wrapPrim h "hGetChar" (hGetCharC h)
       if c < 0 then
          ioError (EOFError "hGetChar" h)
        else
          return (toEnum c)

--nhc currently doesn't implement semi-closed handles.
--Hence using hGetContents twice for the same handle will lead
--to obscure results.

hGetContents :: Handle -> IO String
hGetContents h =
    do c <- wrapPrim h "hGetContents" (hGetCharC h)
       if c < 0 then
          return []
        else
          return (toEnum c : unsafePerformIO (hGetContents h))

{-
IO (\world -> Right (primHGetStr h))
primHGetStr primitive 2 :: Handle -> [Char]
-}

hGetFileName :: Handle -> Maybe String
hGetFileName h = unsafePerformIO $ do
     cs <- wrapPrim h "hGetFileName" (hGetFileNameC h)
     s <- peekCString cs
     return (Just s)


hGetLine              :: Handle -> IO String
#ifndef __HADDOCK__
hGetLine h             = do c  <- hGetChar h
                            case c of
                              '\n' -> return []
                               _   -> do cs <- hGetLine h
                                         return (c:cs)
#endif
hGetPosn :: Handle -> IO HandlePosn
hGetPosn h = do pos <- hTell h
                return (HandlePosn h pos)

hIsEOF :: Handle -> IO Bool
hIsEOF h = wrapPrim h "hIsEOF" (hIsEOFC h)

hPrint                :: Show a => Handle -> a -> IO ()
hPrint h a             = hPutStr h (show a)

hPutChar              :: Handle -> Char -> IO ()
hPutChar h c           = wrapPrim h "hPutChar" (hPutCharC h c)

-- This is the definitely working, but inefficient, version.
hPutStr               :: Handle -> String -> IO ()
hPutStr h []           = return ()
hPutStr h (x:xs)       = hPutChar h x >> hPutStr h xs

-- This version does string-packing in chunks on the
-- C-side, improving the performance no matter how large
-- the string is.

-- We cannot yet use this when dealing with traced strings -
-- they have a different data representation that needs to be
-- coded explicitly in C.

--foreign import ccall "hPutStrC" hPutStrC :: Handle -> String -> IO ()

--hPutStr               :: Handle -> String -> IO ()
--hPutStr h []           = return ()
--hPutStr h xs@(x:_)     = hPutStrC h xs

-- Note: we rely on pattern-matching here to force the evaluation of
-- the first cons in the string - for some unknown reason this is
-- important for the runtime system.  The obvious simple alternative:
--      hPutStr = hPutStrC
-- gives an "unevaluated tag in TABLESWITCH" runtime crash.

hPutStrLn      :: Handle -> String -> IO ()
hPutStrLn h s   =  do hPutStr h s
                      hPutChar h '\n'

hSeek                 :: Handle -> SeekMode -> Integer -> IO ()
hSeek h s i = wrapPrim h "hSeek" (hSeekC h (fromEnum s) (fromInteger i))

hTell :: Handle -> IO Integer
hTell h = wrapPrim h "hTell" (hTellC h) >>= return .toInteger


hSetBuffering         :: Handle  -> BufferMode -> IO ()
hSetBuffering h b = wrapPrim h "hSetBuffering" (hSetBufferingC h (fromEnum b))

hGetBuffering :: Handle -> IO BufferMode
hGetBuffering h = wrapPrim h "hGetBuffering" (hGetBufferingC h) >>= return . toEnum

hSetPosn              :: HandlePosn -> IO ()
hSetPosn (HandlePosn h p) = hSeek h AbsoluteSeek p

ioeGetErrorString :: IOError -> String
ioeGetErrorString (IOError op _ _ errno)  = op
ioeGetErrorString (EOFError op handle)    = "EOF"
ioeGetErrorString (PatternError loc)      = loc
ioeGetErrorString (UserError loc str)     = str
ioeGetErrorString _ = "unusual IO error"

ioeGetFileName        :: IOError -> Maybe FilePath
ioeGetFileName (IOError cmd file@(Just _) _        errno)   = file

ioeGetFileName ioerror = case ioeGetHandle ioerror of
               Nothing -> Nothing
               Just h -> hGetFileName h

ioeGetHandle :: IOError -> Maybe Handle
ioeGetHandle (IOError cmd maybefile maybehandle errno) = maybehandle
ioeGetHandle (EOFError op handle)                      = Just handle
ioeGetHandle _                                         = Nothing

data IOMode      =  ReadMode | WriteMode | AppendMode | ReadWriteMode
                     deriving (Eq, Ord, Ix, Bounded, Enum, Read, Show)

isEOF                 :: IO Bool
isEOF                  = hIsEOF stdin

isEOFError            :: IOError -> Bool
isEOFError (EOFError fun file) = True
isEOFError ioerror = False

isAlreadyExistsError  :: IOError -> Bool
isAlreadyExistsError (IOError _ _ _ errno) = errno `elem` alreadyexists

isDoesNotExistError   :: IOError -> Bool
isDoesNotExistError (IOError _ _ _ errno)  = errno `elem` doesnotexist

isAlreadyInUseError   :: IOError -> Bool
isAlreadyInUseError (IOError _ _ _ errno)  = errno `elem` alreadyinuse

isFullError           :: IOError -> Bool
isFullError         (IOError _ _ _ errno)  = errno `elem` full

isIllegalOperation    :: IOError -> Bool
isIllegalOperation  (IOError _ _ _ errno)  = errno `elem` illegalop

isPermissionError     :: IOError -> Bool
isPermissionError  (IOError _ _ _ errno)   = errno `elem` nopermission

isUserError  :: IOError -> Bool
isUserError (UserError loc str) = True
isUserError ioerror = False

hWaitForInput         :: Handle -> Int -> IO Bool
hWaitForInput h n      = error "Not defined: hWaitForInput"

hReady                :: Handle -> IO Bool
hReady h               = hWaitForInput h 0

hLookAhead            :: Handle -> IO Char
hLookAhead h           = error "Not defined: hLookAhead"

hIsOpen               :: Handle -> IO Bool
hIsOpen h              = error "Not defined: hIsOpen"

hIsClosed             :: Handle -> IO Bool
hIsClosed h            = error "Not defined: hIsClosed"

hIsReadable           :: Handle -> IO Bool
hIsReadable h          = error "Not defined: hIsReadable"

hIsWritable           :: Handle -> IO Bool
hIsWritable h          = error "Not defined: hIsWritable"

hIsSeekable           :: Handle -> IO Bool
hIsSeekable h          = error "Not defined: hIsSeekable"

--  All this was incorrect.  It opened a small gap between the
--  allocation of the ForeignObj, and its attachment into the program
--  graph with addrToFO.  Hence occasionally, depending on exact
--  GC time, we got seg-faults.
#ifndef __HADDOCK__
-- foreign import openFileC :: CString -> Int -> IO Addr
-- #if !defined(TRACING)
-- foreign import "addrToHandle" addrToHandle :: Addr -> IO Handle
-- #else
-- foreign import "addrToHandle" addrToFO :: Addr -> IO ForeignObj
-- addrToHandle a = addrToFO a >>= return . Handle
-- #endif
#endif
-- openFile              :: FilePath -> IOMode -> IO Handle
-- openFile fp iomode = do
--     a <- openFileC (toCString fp) (fromEnum iomode)
--     if a==nullAddr then do
--         errno <- getErrNo
--         throwIOError ("openFile "++show iomode) (Just fp) Nothing errno
--       else do
--         addrToHandle a

-- Note: the primitive openFileC returns an Addr that is in fact a
-- pointer to the C structure representing a ForeignObj.  This is
-- how we can cast it to a Handle later on.  The only reason we
-- don't return the Handle directly is so that we can test if it
-- is NULL (which can only be done in the Addr type), indicating a
-- failure to open the requested file.

-- Further note: the really correct way to do things would be to
-- return the Addr of the FILE* allocated by fopen(), then to
-- turn it into a ForeignObj by adding the finaliser closeFile.
-- Unfortunately, we haven't got finalisers of type IO () working yet,
-- so the finaliser has to be attached in the C world rather than
-- the Haskell world for the moment.

openFile              :: FilePath -> IOMode -> IO Handle
openFile fp iomode =
    do a <- withCString fp (\cs -> openFileC cs (fromEnum iomode))
       if a==nullPtr then do
          errno <- getErrNo
          throwIOError ("openFile "++show iomode) (Just fp) Nothing errno
        else makeHandle a

--openSocket                 :: String -> Int -> SocketType -> IO Handle
--openSocket host port stype = primOpenSocket host port stype

data SeekMode    =  AbsoluteSeek | RelativeSeek | SeekFromEnd
                     deriving (Eq, Ord, Ix, Bounded, Enum, Read, Show)

instance Show Handle where
  showsPrec _ h = showString "(Handle for file " . shows (hGetFileName h)
        . showString ")"

  showsType h = showString "Handle"

instance Show HandlePosn where
  showsPrec _ (HandlePosn h p)
        = showString "(HandlePosn for file "
        . shows (hGetFileName h) . showString ")"

  showsType h = showString "HandlePosn"


try                   :: IO a -> IO (Either IOError a)
try f                  = catch (do r <- f
                                   return (Right r))
                               (return . Left)

stdout, stderr, stdin :: Handle
stdout = unsafePerformIO (makeHandle (stdoutC ()))
stderr = unsafePerformIO (makeHandle (stderrC ()))
stdin  = unsafePerformIO (makeHandle (stdinC ()))

makeHandle :: Ptr () -> IO Handle
makeHandle a = do f <- newForeignPtr_ a
                  let h = Handle f
                  addForeignPtrFinalizer (hCloseC h) f
                  return h

mkIOError :: String -> Maybe FilePath -> Maybe Handle -> Int -> IOError
mkIOError str mf mh err = IOError str mf mh (toEnum err)

throwIOError :: String -> Maybe FilePath -> Maybe Handle -> Int -> IO a
throwIOError str mf mh err = ioError (mkIOError str mf mh err)

fixIO :: (a -> IO a) -> IO a
fixIO k = do
    ref <- newIORef (error "NonTermination")--(throw NonTermination)
    ans <- unsafeInterleaveIO (readIORef ref)
    result <- k ans
    writeIORef ref result
    return result
