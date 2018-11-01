import System.IO

import Foreign
import Data.PackedString
#ifndef __HADDOCK__
foreign import primEqHandleC :: Handle -> Handle -> Bool
foreign import hCloseC  :: () -> FunPtr (Ptr () -> IO ())
foreign import primHFileSizeC :: Handle -> Int
foreign import hGetCharC :: Handle -> Int
foreign import hFlushC :: Handle -> ()
foreign import hGetFileNameC :: Handle -> PackedString
foreign import hIsEOFC  :: Handle -> Bool
foreign import hPutCharC :: Handle -> Char -> ()
foreign import hSeekC  :: Handle -> Int -> Int -> ()
foreign import hSetBufferingC :: Handle -> Int -> ()
foreign import hTellC  :: Handle -> Int
foreign import openFileC  :: PackedString -> Int -> Ptr ()
foreign import stdoutC  :: () -> Ptr ()
foreign import stderrC :: () -> Ptr ()
foreign import stdinC :: () -> Ptr ()
#endif

