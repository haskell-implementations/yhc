module Data.Array (
     module Data.Ix  -- export all of Ix
    ,Array
    ,array
    ,listArray
    ,(!)
    ,bounds
    ,indices
    ,elems
    ,assocs
    ,accumArray
    ,(//)
    ,accum
--  ,amap   -- now fmap (instance Functor)
    ,ixmap
    ) where

import Data.Ix
import YHC.Primitive
import YHC.Internal(unsafePerformIO)
import YHC.Exception
-- import PreludeBuiltin(Vector)



-- Originally From Accum.hs


accum                 :: (Ix a) => (b -> c -> b) -> Array a b -> [(a,c)]
                                   -> Array a b
accum f              =  foldl (\a (i,v) -> a // [(i,f (a!i) v)])

{-
-- Possible improved implementation?
-- Gives sig-11 core dump at the mo!


accum f (MkArray b v) as =
  unsafePerformIO ( do
    v' <- primCopyVectorC v
    mapM_ (\(ix,elt)-> do
             let i = index b ix
             val <- primVectorIndexC v' i
             primUpdateVectorC i (_E (f val elt)) v')
          as
    return (MkArray b v')
  )
-}


-- Originally From AccumArray.hs


accumArray            :: (Ix a) => (b -> c -> b) -> b -> (a,a) -> [(a,c)]
                                   -> Array a b
accumArray f z b      =  accum f (array b [(i,z) | i <- range b])


-- Originally From AIndex.hs


infixl 9  !

-- foreign import ccall "primVectorIndexC" primIndex :: Vector a -> Int -> a
-- primVectorIndexC primitive 2 :: Vector a -> Int -> a

primIndex v i = primVectorIndexC v i


(!)                   :: (Ix a) => Array a b -> a -> b
(!) (MkArray b v) i   =  primIndex v (arIndex b i)



-- Originally From AMap.hs


--amap                  :: (Ix a) => (b -> c) -> Array a b -> Array a c
--amap f a              =  array b [(i, f (a!i)) | i <- range b]
--                         where b = bounds a

instance (Ix a) => Functor (Array a) where
    fmap f a  =  array b [(i, f (a!i)) | i <- range b]
                 where b = bounds a


-- Originally From ArrayFun.hs


array :: (Ix a) => (a,a) -> [(a,b)] -> Array a b
array b ivs =
    if and [inRange b i | (i,_) <- ivs]
    then unsafePerformIO ( do
           v <- primNewVectorC (rangeSize b)
                               (_E $ throw $ ArrayException $ UndefinedElement "")
           mapM_ (\(i,a)-> primUpdateVectorC (arIndex b i) (_E a) v) ivs
           return (MkArray b v)
         )
    else throw $ ArrayException $ IndexOutOfBounds ""

-- Originally From ArrPrec.hs

-- Precedence of the 'array' function is that of application itself
arrPrec :: Int
arrPrec = 10


-- Originally From Assocs.hs


assocs                :: (Ix a) => Array a b -> [(a,b)]
assocs a              =  [(i,a!i) | i <- indices a]


-- Originally From Bounds.hs


bounds                :: (Ix a) => Array a b -> (a,a)
bounds (MkArray b _)  =  b


-- Originally From DArray.hs


data  (Ix a)    => Array a b = MkArray (a,a) (Vector b)


-- Originally From Elems.hs


elems                 :: (Ix a) => Array a b -> [b]
elems a               =  [a!i | i <- indices a]


-- Originally From Eq_Array.hs

--import Assocs

instance  (Ix a, Eq b)  => Eq (Array a b)  where
 -- a == a'             =  assocs a == assocs a'
    a == a'             =  bounds a == bounds a' && elems a == elems a'


-- Originally From Indices.hs


indices               :: (Ix a) => Array a b -> [a]
indices               =  range . bounds


-- Originally From IxMap.hs


ixmap                 :: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c
                                         -> Array a c
ixmap b f a           = array b [(i, a ! f i) | i <- range b]


-- Originally From ListArray.hs


listArray             :: (Ix a) => (a,a) -> [b] -> Array a b
listArray b vs        =  array b (zipWith (\ a b -> (a,b)) (range b) vs)


-- Originally From LowVector.hs
{-foreign import ccall primCopyVectorC :: Vector a -> IO (Vector a)
foreign import ccall primNewVectorC :: Int -> _E a -> IO (Vector a)
foreign import ccall primVectorIndexC :: Vector a -> Int -> IO a
foreign import ccall primUpdateVectorC :: Int -> _E a -> Vector a -> IO ()
-}



-- Originally From Ord_Array.hs
instance  (Ix a, Ord b) => Ord (Array a b)  where
    a <=  a'            =  assocs a <=  assocs a'


-- Originally From Read_Array.hs
instance  (Ix a, Read a, Read b) => Read (Array a b)  where
    readsPrec p = readParen (p > arrPrec)
           (\r -> [(array b as, u) | ("array",s) <- lex r,
                                     (b,t)       <- readsPrec (arrPrec+1) s,
                                     (as,u)      <- readsPrec (arrPrec+1) t   ])



-- Originally From Replace.hs


infixl 9  //

(//)                  :: (Ix a) => Array a b -> [(a,b)] -> Array a b
(MkArray b v) // us   = unsafePerformIO (do
              v' <- primCopyVectorC v
              mapM_ (\(ix,elt)-> primUpdateVectorC (arIndex b ix)
                                                               (_E elt)
                                                               v')
                                us
              return (MkArray b v')
            )


arIndex :: (Ix a) => (a,a) -> a -> Int
arIndex b ix
    | inRange b ix = index b ix
    | otherwise    = throw (ArrayException (IndexOutOfBounds ""))

-- Originally From Show_Array.hs
instance  (Ix a, Show a, Show b) => Show (Array a b)  where
    showsPrec p a = showParen (p > arrPrec)
                      ( showString "array "
                      . showsPrec (arrPrec+1) (bounds a) . showChar ' '
                      . showsPrec (arrPrec+1) (assocs a)
                      )

    showsType a = showString " (Array " .  (showsType . fst . bounds) a
                  . showChar ' ' . (showsType . head . elems) a . showChar ')'

