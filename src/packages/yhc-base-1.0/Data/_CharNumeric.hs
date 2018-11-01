-- Code that sits between Char and Numeric, put here due to cyclic dependency issues

#ifndef __HADDOCK__
module Data._CharNumeric where

-------------------------------------------------------------------------------------------------
-- Char
-------------------------------------------------------------------------------------------------
  
-- Originally From Chr.hs  

chr   :: Int -> Char
chr    =  toEnum

-- Originally From Ord.hs  

ord   :: Char -> Int
ord    =  fromEnum

-- Originally From DigitToInt.hs  

digitToInt     :: Char -> Int
digitToInt c
  | isDigit c             = ord c - ord '0'
  | c >= 'a' && c <= 'f'  = ord c - ord 'a' + 10
  | c >= 'A' && c <= 'F'  = ord c - ord 'A' + 10
  | otherwise             = error "Char.digitToInt: not a digit" 
  
-- Originally From IsDigit.hs  

isDigit     :: Char -> Bool
isDigit c       = c >= '0'   &&  c <= '9' 
  
  
-- Originally From IsHexDigit.hs  

isHexDigit      :: Char -> Bool
isHexDigit c   = (c >= '0' && c <= '9')
              || (c >= 'A' && c <= 'F')
              || (c >= 'a' && c <= 'f')

  
-- Originally From IsOctDigit.hs  

isOctDigit      :: Char -> Bool
isOctDigit c       = c >= '0'   &&  c <= '7' 
  

-------------------------------------------------------------------------------------------------
-- Numeric
-------------------------------------------------------------------------------------------------

-- Originally From ReadDec.hs  

readDec :: (Integral a) => ReadS a
readDec = readInt 10 isDigit digitToInt

-- Originally From ReadOct.hs  

readOct :: (Integral a) => ReadS a
readOct = readInt  8 isOctDigit digitToInt

  
-- Originally From ReadHex.hs  

readHex :: (Integral a) => ReadS a
readHex  =  readInt 16 isHexDigit digitToInt

-- Originally From ReadInt.hs  

readInt :: (Integral a) => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
readInt radix isDig digToInt s =
    [(foldl1 (\n d -> n * radix + d) (map (fromIntegral . digToInt) ds), r)
    | (ds,r) <- nonnull isDig s ]
  
  
-- Originally From Nonnull.hs  

nonnull         :: (Char -> Bool) -> ReadS String

nonnull p s     =  [(cs,t) | (cs@(_:_),t) <- [span p s]]

#endif

