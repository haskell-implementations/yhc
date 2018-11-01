{-# OPTIONS_YHC --cpp #-}
module Foreign
    (module Foreign.Int,
     module Foreign.Word,
     module Foreign.Ptr,
     module Foreign.ForeignPtr,
     module Foreign.StablePtr,
     module Foreign.Storable,
     module Foreign.Marshal,
     unsafePerformIO
    ) where

import YHC.Internal(unsafePerformIO)

import Foreign.Int
import Foreign.Word
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.StablePtr
import Foreign.Storable
import Foreign.Marshal