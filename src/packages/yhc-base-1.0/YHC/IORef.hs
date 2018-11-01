module YHC.IORef
    (IORef,
     newIORef,
     readIORef,
     writeIORef) where
     
import YHC.Primitive

newIORef :: a -> IO (IORef a)
newIORef = primNewIORefC

readIORef :: IORef a -> IO a
readIORef = primReadIORefC

writeIORef :: IORef a -> a -> IO ()
writeIORef = primWriteIORefC