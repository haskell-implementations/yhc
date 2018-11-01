module Control.Concurrent.MVar(module Control.Concurrent.MVar, MVar) where

import YHC.Primitive

-- create a new empty mvar
newEmptyMVar :: IO (MVar a)
newEmptyMVar = primNewMVar

-- create a new mvar with a particular value
newMVar :: a -> IO (MVar a)
newMVar a = do mv <- newEmptyMVar
               putMVar mv a
               return mv

-- take the value of a mvar, will block if the mvar is empty
takeMVar :: MVar a -> IO a
takeMVar mv = primTakeMVar mv

-- put a value into an mvar, will block if the mvar is not empty
putMVar :: MVar a -> a -> IO ()
putMVar mv a = do
    ok <- primPutMVar mv a
    if ok then return ()
          else error "putMVar: MVar already contains a value!"

-- read the value from the mvar but leave it in there.
readMVar :: MVar a -> IO a
readMVar mv = primReadMVar mv

-- swap the value of the mvar for another one
swapMVar :: MVar a -> a -> IO a
swapMVar mv a = primSwapMVar mv a

-- returns whether an mvar is emtpy or not
isEmptyMVar :: MVar a -> IO Bool
isEmptyMVar mv = primIsEmptyMVar mv

-- try and take the value from an mvar, or nothing
tryTakeMVar :: MVar a -> IO (Maybe a)
tryTakeMVar mv = primTryTakeMVar mv

-- try and put the value to an mvar, true if success, false otherwise
tryPutMVar :: MVar a -> a -> IO Bool
tryPutMVar mv a = primPutMVar mv a





