{-# OPTIONS_YHC --cpp #-}
module Foreign.C.Error
  ( getErrNo
--  , mkIOError
--  , throwIOError
  ) where

import YHC.ErrNo
import YHC.Primitive

getErrNo :: IO Int
getErrNo = getErrorNo
