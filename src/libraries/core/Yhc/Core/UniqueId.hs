{- |
    This module implements unique ID's in Yhc.Core.

    The intention is that a program can use this interface to a unique ID quite cheaply.
    Or an existing state monad can be reused.
-}

module Yhc.Core.UniqueId where

import Control.Monad.State


-- store the value to use next
class UniqueId a where
    getId :: a -> Int
    putId :: Int -> a -> a


instance UniqueId Int where
    getId = id
    putId = const


class Monad m => UniqueIdM m where
    getIdM :: m Int
    putIdM :: Int -> m ()


instance UniqueId a => UniqueIdM (State a) where
    getIdM = liftM getId get
    putIdM n = modify (putId n)


nextId :: UniqueIdM m => m Int
nextId = do i <- getIdM
            putIdM (i+1)
            return i
