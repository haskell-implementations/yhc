
module Yhc.Core.Internal.Binary where

import System.IO
import Data.Char
import Control.Monad


class Binary a where
    put_   :: Handle -> a -> IO ()
    get    :: Handle -> IO a


writeBinary :: Binary a => FilePath -> a -> IO ()
writeBinary file x = do
    hndl <- openBinaryFile file WriteMode
    put_ hndl x
    hClose hndl

readBinary :: Binary a => FilePath -> IO a
readBinary file = do
    hndl <- openBinaryFile file ReadMode
    res <- get hndl
    hClose hndl
    return res


putByte :: Handle -> Int -> IO ()
putByte hndl x = hPutChar hndl (chr x)
getByte :: Handle -> IO Int
getByte hndl = liftM ord $ hGetChar hndl


instance Binary a => Binary [a] where
    put_ bh [] = putByte bh 0
    put_ bh xs = do putByte bh (length a); mapM_ (put_ bh) a; put_ bh b
        where (a,b) = splitAt 100 xs
    
    get bh         = do h <- getByte bh
                        case h of
                          0 -> return []
                          _ -> do xs <- replicateM h (get bh)
                                  ys <- get bh
                                  return (xs ++ ys)

instance Binary a => Binary (Maybe a) where
    put_ bh Nothing = putByte bh 0
    put_ bh (Just x) = putByte bh 1 >> put_ bh x
    
    get bh = do h <- getByte bh
                case h of
                    0 -> return Nothing
                    1 -> liftM Just $ get bh


instance (Binary a, Binary b) => Binary (a,b) where
    put_ h (a,b) = put_ h a >> put_ h b
    get h = do a <- get h
               b <- get h
               return (a,b)

instance (Binary a, Binary b, Binary c) => Binary (a,b, c) where
    put_ h (a,b,c) = put_ h a >> put_ h b >> put_ h c
    get h = do a <- get h
               b <- get h
               c <- get h
               return (a,b,c)

instance Binary Bool where
    put_ hndl x = hPutChar hndl (if x then '1' else '0')
    get hndl = hGetChar hndl >>= return . (== '1')


instance Binary Char where
    put_ = hPutChar
    get = hGetChar


-- TODO: horrible versions
-- a quick hacky, replace and integrate with
-- the Binary from Yhc.ByteCode

showPut :: Show a => Handle -> a -> IO ()
showPut h x = put_ h (show x)

showGet :: Read a => Handle -> IO a
showGet h = liftM read $ get h

instance Binary Int where{put_ = showPut; get = showGet}
instance Binary Integer where{put_ = showPut; get = showGet}
instance Binary Float where{put_ = showPut; get = showGet}
instance Binary Double where{put_ = showPut; get = showGet}
    

