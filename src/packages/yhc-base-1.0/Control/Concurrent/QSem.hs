module Control.Concurrent.QSem where

import Control.Concurrent.MVar

newtype QSem = QSem (MVar (Int, [MVar ()]))

newQSem :: Int -> IO QSem
newQSem init = do
  sem <- newMVar (init, [])
  return (QSem sem)

waitQSem :: QSem -> IO ()
waitQSem (QSem sem) = do
  (avail,blocked) <- takeMVar sem
  if avail > 0 then
    putMVar sem (avail-1,[])
   else do
    block <- newEmptyMVar
    putMVar sem (0, blocked++[block])
    takeMVar block

signalQSem :: QSem -> IO ()
signalQSem (QSem sem) = do
  (avail,blocked) <- takeMVar sem
  case blocked of
    [] -> putMVar sem (avail+1,[])
    (block:blocked') -> do
        putMVar sem (0, blocked')
        putMVar block ()
