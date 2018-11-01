{-# OPTIONS_YHC --redefine --prelude #-}
module Prelude
    (
{-
     (->),Char,Int,Integer,Float,Double,
     String,
     Either(..),[](..),Maybe(..),Bool(..),Ordering(..),
     Bounded(..),Enum(..),Eq(..),Ord(..),Read(..),Show(..),Integral(..),Eq(..),Num(..),Monad(..),Functor(..),
     Fractional(..),Real(..),RealFrac(..),Floating(..),RealFloat(..),

     ()(..),
     (,)(..),
     (,,)(..),
     (,,,)(..),
     (,,,,)(..),
     (,,,,,)(..),
     (,,,,,,)(..),
     (,,,,,,,)(..),
     (,,,,,,,,)(..),
     (,,,,,,,,,)(..),
     (,,,,,,,,,,)(..),
     (,,,,,,,,,,,)(..),
     (,,,,,,,,,,,,)(..),
     (,,,,,,,,,,,,,)(..),
     (,,,,,,,,,,,,,,)(..),
     IOError(..), ioError, userError, FilePath,catch,
     IO,

     maybe,
     fst,snd,

     all,and,any,(++),break,concat,concatMap,cycle,drop,dropWhile,elem,filter,foldl,foldl1,foldr,
     foldr1,head,(!!),init,iterate,last,length,lines,lookup,map,maximum,minimum,notElem,null,
     or,product,repeat,replicate,reverse,scanl,scanl1,scanr,scanr1,span,splitAt,sum,tail,
     take,takeWhile,unlines,unwords,unzip,unzip3,words,zip,zip3,zipWith,zipWith3,

     ($),($!),flip,id,const,otherwise,error,undefined,gcd,(.),seq,

     fromIntegral,numericEnumFrom,numericEnumFromTo,numericEnumFromThen,numericEnumFromThenTo,(^),(^^),even,odd,subtract,
     (&&),(||),not,
     ReadS,read,reads,readParen,lex,
     ShowS,print,shows,showString, showParen, showChar,
     mapM,mapM_,sequence,sequence_,(=<<),
     putChar,putStr,putStrLn,

     _seq,_filter,_foldr,_fromEnum, _toEnum, _enumFromTo, _enumFromThen, _enumFromThenTo,

     Rational,
     (%)
-}
    _filter,_foldr,
    -- module PreludeList:
    map, (++), filter, concat,
    head, last, tail, init, null, length, (!!),
    foldl, foldl1, scanl, scanl1, foldr, foldr1, scanr, scanr1,
    iterate, repeat, replicate, cycle,
    take, drop, splitAt, takeWhile, dropWhile, span, break,
    lines, words, unlines, unwords, reverse, and, or,
    any, all, elem, notElem, lookup,
    sum, product, maximum, minimum, concatMap,
    zip, zip3, zipWith, zipWith3, unzip, unzip3,
    -- module PreludeText:
    ReadS, ShowS,
    Read(..),
    Show(..),
    reads, shows, read, lex,
    showChar, showString, readParen, showParen, showType,
    -- module PreludeIO:
    _readCon,_readCon0,_readField,_readConArg,_readFinal,_readConInfix,
    FilePath, IOError(..), ioError, userError, catch,
    putChar, putStr, putStrLn, print,
    getChar, getLine, getContents, interact,
    readFile, writeFile, appendFile, readIO, readLn,
    -- defined here:
    Bool(..),
    Maybe(..),
    Either(..),
    Ordering(..),
    Char, String, Int, Integer, Float, Double, Rational, (%), IO,

--      These built-in types are defined in the Prelude, but
--      are denoted by built-in syntax, and cannot legally
--      appear in an export list.
--  But they do here, because the transformed module will need the exports
--  List type:
     [](..),
--  Tuple types: up to 20
     ()(..),
     (,)(..),
     (,,)(..),
     (,,,)(..),
     (,,,,)(..),
     (,,,,,)(..),
     (,,,,,,)(..),
     (,,,,,,,)(..),
     (,,,,,,,,)(..),
     (,,,,,,,,,)(..),
     (,,,,,,,,,,)(..),
     (,,,,,,,,,,,)(..),
     (,,,,,,,,,,,,)(..),
     (,,,,,,,,,,,,,)(..),
     (,,,,,,,,,,,,,,)(..),
     (,,,,,,,,,,,,,,,)(..),
     (,,,,,,,,,,,,,,,,)(..),
     (,,,,,,,,,,,,,,,,,)(..),
     (,,,,,,,,,,,,,,,,,,)(..),
     (,,,,,,,,,,,,,,,,,,,)(..),
--  Functions:
    (->),

    Eq(..),
    Ord(..),
    Enum(..),
    Bounded(..),
    Num(..),
    Real(..),
    Integral(..),
    Fractional(..),
    Floating(..),
    RealFrac(..),
    RealFloat(..),
    Monad(..),
    Functor(..),
    mapM, mapM_, sequence, sequence_, (=<<),
    maybe, either,
    (&&), (||), not, otherwise,
    subtract, even, odd, gcd, lcm, (^), (^^),
    numericEnumFrom,numericEnumFromTo,numericEnumFromThen,numericEnumFromThenTo,
    fromIntegral, realToFrac,
    fst, snd, curry, uncurry, id, const, (.), flip, ($), until,
    asTypeOf, error, undefined,
    seq, ($!),

     _seq,_filter,_foldr,_fromEnum, _toEnum, _enumFromTo, _enumFromThen, _enumFromThenTo,
     _enumIndex,_enumRange,_enumInRange,


    _noMethodError,_nonTermination,_patternMatchFail,_recConError,
    _recSelError,_recUpdError
    )
    where

import PreludeBuiltin((->),Char,Int,Integer,Float,Double)
import System.Exit(exitWith,ExitCode(..))
import Numeric
import YHC.Internal(unsafePerformIO,IO(..),World(..))
import YHC.ErrNo
import System.IO
import Data.Char
import Data.Ratio
import Debug.Trace(trace)
import PreludeAux
import YHC.Primitive
import Foreign.C
import YHC.Exception

----------------------------------------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------------------------------------

data Either a b = Left a | Right b
data Maybe a = Nothing | Just a
data (,)              a b                                          = (,)                   a b
data (,,)             a b c                                        = (,,)                  a b c
data (,,,)            a b c d                                      = (,,,)                 a b c d
data (,,,,)           a b c d e                                    = (,,,,)                a b c d e
data (,,,,,)          a b c d e f                                  = (,,,,,)               a b c d e f
data (,,,,,,)         a b c d e f g                                = (,,,,,,)              a b c d e f g
data (,,,,,,,)        a b c d e f g h                              = (,,,,,,,)             a b c d e f g h
data (,,,,,,,,)       a b c d e f g h i                            = (,,,,,,,,)            a b c d e f g h i
data (,,,,,,,,,)      a b c d e f g h i j                          = (,,,,,,,,,)           a b c d e f g h i j
data (,,,,,,,,,,)     a b c d e f g h i j k                        = (,,,,,,,,,,)          a b c d e f g h i j k
data (,,,,,,,,,,,)    a b c d e f g h i j k l                      = (,,,,,,,,,,,)         a b c d e f g h i j k l
data (,,,,,,,,,,,,)   a b c d e f g h i j k l m                    = (,,,,,,,,,,,,)        a b c d e f g h i j k l m
data (,,,,,,,,,,,,,)  a b c d e f g h i j k l m n                  = (,,,,,,,,,,,,,)       a b c d e f g h i j k l m n
data (,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o                = (,,,,,,,,,,,,,,)      a b c d e f g h i j k l m n o
data (,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p             = (,,,,,,,,,,,,,,,)     a b c d e f g h i j k l m n o p
data (,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q          = (,,,,,,,,,,,,,,,,)    a b c d e f g h i j k l m n o p q
data (,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r       = (,,,,,,,,,,,,,,,,,)   a b c d e f g h i j k l m n o p q r
data (,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s    = (,,,,,,,,,,,,,,,,,,)  a b c d e f g h i j k l m n o p q r s
data (,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t = (,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t

----------------------------------------------------------------------------------------------------
-- String
----------------------------------------------------------------------------------------------------

type String = [Char]

----------------------------------------------------------------------------------------------------
-- Classes
----------------------------------------------------------------------------------------------------

class Bounded a where
  minBound, maxBound :: a

-- Enumeration

class Enum a where
    succ, pred          :: a -> a
    toEnum      :: Int -> a
    fromEnum        :: a -> Int
    enumFrom        :: a -> [a]     -- [n..]
    enumFromThen    :: a -> a -> [a]    -- [n,n'..]
    enumFromTo      :: a -> a -> [a]    -- [n..m]
    enumFromThenTo  :: a -> a -> a -> [a]   -- [n,n'..m]

    succ                = toEnum . (+1) . fromEnum
    pred                = toEnum . subtract 1 . fromEnum

    enumFromTo n m  = let ni = fromEnum n
                          mi = fromEnum m
                      in case compare ni mi of
                           LT -> _enumFromToIncC ni 1 mi
                           EQ -> n:[]
                           GT -> []

    enumFromThenTo n n' m = let ni  = fromEnum n
                                ni' = fromEnum n'
                                mi  = fromEnum m
                                step = ni' - ni
                            in if step >= 0 then
                                   _enumFromToIncC ni step mi
                               else
                                   _enumFromToDecC ni step mi


_enumFromToDecC :: (Enum a) => Int -> Int -> Int -> [a]
_enumFromToDecC n s m =
  case compare n m of
    LT -> []
    EQ -> toEnum n : []
    GT -> toEnum n : _enumFromToDecC (n+s) s m


_enumFromToIncC :: (Enum a) => Int -> Int -> Int -> [a]
_enumFromToIncC n s m =
  case compare n m of
    LT -> toEnum n : _enumFromToIncC (n+s) s m
    EQ -> toEnum n : []
    GT -> []

-- Read

class  Read a  where
    readsPrec  :: Int -> ReadS a
    readList   :: ReadS [a]

    readList = readParen False (\r -> [pr | ("[",s) <- lex r
                                      , pr      <- readl s])
      where readl s  = [([],t)   | ("]",t) <- lex s] ++
                       [(x:xs,u) | (x,t)  <- readsPrec 0 s
                                 , (xs,u) <- readl' t]
            readl' s = [([],t)   | ("]",t) <- lex s] ++
                       [(x:xs,v) | (",",t)  <- lex s
                                 , (x,u) <- readsPrec 0 t
                                 , (xs,v) <- readl' u]

-- Show

class  Show a  where
        showsPrec :: Int -> a -> ShowS
        show      :: a -> String
        showList  :: [a] -> ShowS
        showsType :: a -> ShowS

        showsPrec _ x s = show x ++ s
        show x          = showsPrec 0 x ""

        showList [] = showString "[]"
        showList (x:xs) =
          showChar '[' . showsPrec 0 x . showl xs
              where showl []     = showChar ']'
                    showl (x:xs) = showString "," . showsPrec 0 x . showl xs

-- Equality

infix  4  ==, /=

class Eq a  where
    (==), (/=)      :: a -> a -> Bool

    x /= y      =  not (x == y)
    x == y      =  not (x /= y)

-- Ordering

infix  4  <, <=, >=, >

class  (Eq a) => Ord a  where
    compare     :: a -> a -> Ordering
    (<), (<=), (>=), (>):: a -> a -> Bool
    max, min        :: a -> a -> a

    compare x y
      | x == y    = EQ
      | x <= y    = LT
      | True      = GT

    x <= y      = compare x y /= GT
    x <  y      = compare x y == LT
    x >= y      = compare x y /= LT
    x >  y      = compare x y == GT

    max x y | x >= y    =  x
            | True  =  y
    min x y | x <= y    =  x
            | True  =  y

-- Numeric

infixl 7  *
infixl 6  +

#ifndef __HADDOCK__
prefix negate 6 -   -- WARNING Not standard Haskell
#endif


class  (Eq a, Show a) => Num a  where
    (+), (-), (*)   :: a -> a -> a
    negate      :: a -> a
    abs, signum     :: a -> a
    fromInteger     :: Integer -> a

    x - y               =  x + negate y
    negate x            =  0 - x

infixl 7  /

-- Real numbers

class (Num a, Ord a) => Real a where
  toRational        :: a -> Rational

-- Integral numbers

infixl 7  `quot`, `rem`, `div`, `mod`

class  (Real a, Enum a) => Integral a  where
    quot, rem, div, mod :: a -> a -> a
    quotRem, divMod :: a -> a -> (a,a)
    toInteger       :: a -> Integer

    n `quot` d      =  fst (quotRem n d)
    n `rem`  d      =  snd (quotRem n d)
    n `div`  d          =  fst (divMod  n d)
    n `mod`  d          =  snd (divMod  n d)

    quotRem n d     = (quot n d, rem n d)
    divMod n d          =  if signum r == negate (signum d) then (q-1, r+d)
                           else qr
                 where qr@(q,r) = quotRem n d

-- Fractions

class  (Num a) => Fractional a  where
    (/)         :: a -> a -> a
    recip       :: a -> a
    fromRational    :: Rational -> a

    recip x     =  1 / x
    x / y               = x * recip y

-- Real Fractional Numbers

class  (Real a, Fractional a) => RealFrac a  where
    properFraction  :: (Integral b) => a -> (b,a)
    truncate, round :: (Integral b) => a -> b
    ceiling, floor  :: (Integral b) => a -> b

    truncate x  =  case properFraction x of (m,_) -> m
    round x =  case properFraction x of
            (n,r) ->
                  case compare (abs r - fromRational (1%2)) 0 of
                LT -> n
                EQ -> if even n then n else if r<0 then n-1 else n+1
                GT ->                       if r<0 then n-1 else n+1
    ceiling x   =  case properFraction x of (n,r) -> if r>0 then n+1 else n
    floor x =  case properFraction x of (n,r) -> if r<0 then n-1 else n

-- Floating point

infixr 8  **

class  (Fractional a) => Floating a  where
    pi          :: a
    exp, log, sqrt  :: a -> a
    (**), logBase   :: a -> a -> a
    sin, cos, tan   :: a -> a
    asin, acos, atan    :: a -> a
    sinh, cosh, tanh    :: a -> a
    asinh, acosh, atanh :: a -> a

    x ** y      =  exp (log x * y)
    logBase x y     =  log y / log x
    sqrt x      =  x ** fromRational (1%2)  -- 0.5
    tan  x      =  sin  x / cos  x
    tanh x      =  sinh x / cosh x

-- Real Floating numbers

class  (RealFrac a, Floating a) => RealFloat a  where
    floatRadix      :: a -> Integer
    floatDigits     :: a -> Int
    floatRange      :: a -> (Int,Int)

    decodeFloat     :: a -> (Integer,Int)
    encodeFloat     :: Integer -> Int -> a
    exponent        :: a -> Int
    significand     :: a -> a
    scaleFloat      :: Int -> a -> a
    isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE
            :: a -> Bool
    atan2               :: a -> a -> a

    exponent x      = case decodeFloat x of
                              (m,n) -> if m == 0 then 0 else n + floatDigits x
    significand x   = case decodeFloat x of
                              (m,_) -> encodeFloat m (negate (floatDigits x))
    scaleFloat k x  = case decodeFloat x of
                              (m,n) -> encodeFloat m (n+k)

{-
    atan2 y x = case (signum y, signum x) of
                    ( 0,  1)  -> 0
                    ( 1,  0)  -> pi/2
                    ( 0,(-1)) -> negate pi
                    (-1,  0)  -> negate pi/2
                    ( _,  1)  -> atan (y/x)
                    ( _,(-1)) -> atan (y/x) + pi
                    ( 0,  0)  -> error "Prelude.atan2: atan2 of origin"
-}
    atan2 y x
      | y==0 && x>0    = 0
      | y>0  && x==0   = pi/2
      | y==0 && x<0    = negate pi
      | y<0  && x==0   = negate pi/2
      |         x>0    = atan (y/x)
      |         x<0    = atan (y/x) + pi
      | y==0 && x==0   = error "Prelude.atan2: atan2 of origin"
      | otherwise      = error "Prelude.atan2: not a number"


-- Functions

class Functor f where
    fmap  :: (a -> b) -> f a -> f b


instance Functor IO where
    fmap f x = x >>= (return . f)


-- Monadic

infixl 1 >>, >>=

class Monad m where
    (>>=)   :: m a -> (a -> m b) -> m b
    (>>)    :: m a -> m b -> m b
    return      :: a -> m a
    fail        :: String -> m a

    m >> k  = m >>= \ _ -> k
    fail s      = error s


----------------------------------------------------------------------------------------------------
-- Maybe
----------------------------------------------------------------------------------------------------

maybe                   :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing       =  n
maybe n f (Just x)      =  f x

instance Monad Maybe where
    (Just x) >>= k  = k x
    Nothing  >>= k  = Nothing
    return      = Just
    fail s              = Nothing

instance  Functor Maybe  where
    fmap _ Nothing       = Nothing
    fmap f (Just a)      = Just (f a)


instance Eq a => Eq (Maybe a) where
  Nothing == Nothing = True
  Just a  == Just b  = a == b
  _       == _       = False

instance Ord a => Ord (Maybe a) where
  compare Nothing  Nothing  = EQ
  compare Nothing  (Just b) = LT
  compare (Just a) Nothing  = GT
  compare (Just a) (Just b) = compare a b

instance  (Show a) => Show (Maybe a)  where
  showsPrec p Nothing  = showString "Nothing"
  showsPrec p (Just a) = showParen (p >= 10)
                                 (showString "Just " . showsPrec 10 a)

  showsType a = showString "(Maybe " . showsType ta . showChar ')'
          where (Just ta) = a


instance (Read a) => Read (Maybe a) where
  readsPrec p =  \ r -> readParen False   ( \r -> [(Nothing,t) | ("Nothing",t) <- lex r]) r
                   ++
                readParen (p > 9) ( \r -> [(Just a,v) | ("Just",t) <- lex r,
                                    (a,v) <- readsPrec 10 t]) r



----------------------------------------------------------------------------------------------------
-- Either
----------------------------------------------------------------------------------------------------

either :: (a->c) -> (b->c) -> (Either a b) -> c
either f g (Left x)  = f x
either f g (Right x) = g x

instance  (Show a, Show b) => Show (Either a b)  where
  showsPrec p (Left a) = showParen (p >= 10) (showString "Left " . showsPrec 10 a)
  showsPrec p (Right a) = showParen (p >= 10) (showString "Right " . showsPrec 10 a)

  showsType a = showString "(Either " . (showsType . getLeft)  a . showChar ' '
                      . (showsType . getRight) a . showChar ')'
    where getLeft  (Left a)  = a
          getRight (Right b) = b

instance (Read a, Read b) => Read (Either a b) where
  readsPrec p =  readParen (p > 9)
                   ( \r -> [(Left a,v) | ("Left",t) <- lex r,
                                         (a,v) <- readsPrec 10 t]
                             ++
                           [(Right a,v) | ("Right",t) <- lex r,
                                          (a,v) <- readsPrec 10 t])


----------------------------------------------------------------------------------------------------
-- List
----------------------------------------------------------------------------------------------------

infixr 5 :

data [] a = [] | a : [a]

instance Eq a => Eq [a] where
  []     == []     = True
  (a:as) == (b:bs) = a == b && as == bs
  _      == _      = False

instance Ord a => Ord [a] where
  compare []     []     = EQ
  compare []     (b:bs) = LT
  compare (a:as) []     = GT
  compare (a:as) (b:bs) =
    case compare a b of
        LT -> LT
        EQ -> compare as bs
        GT -> GT

instance Functor [] where
  fmap = map

instance (Read a) => Read [a] where
  readsPrec p = readList

instance (Show a) => Show [a] where
  showsPrec p = showList

  showsType a = showChar '[' . (showsType . head) a . showChar ']'

instance Monad [] where
  m >>= k    =  concat (map k m)
  return x   =  x:[]
  fail s     =  []

all     :: (a -> Bool) -> [a] -> Bool
all p       = and . map p

and     :: [Bool] -> Bool
and         = foldr (&&) True

any     :: (a -> Bool) -> [a] -> Bool
any p       = or . map p

infixr 5 ++

(++)          :: [a] -> [a] -> [a]
[]     ++ ys   =  ys
(x:xs) ++ ys   =  x : (xs ++ ys)

break :: (a -> Bool) -> [a] -> ([a], [a])
break p = span (not . p)

concat          :: [[a]] -> [a]
concat           =  foldr (++) []

concatMap   :: (a->[b]) -> [a] -> [b]
concatMap f = concat . map f

cycle       :: [a] -> [a]
cycle []    = error "Prelude.cycle: empty list"
cycle xs    = xs' where xs' = xs ++ xs'

drop            :: Int -> [a] -> [a]
drop n xs | n<=0         = xs
drop _ []                = []
drop n (x:xs)            = drop (n-1) xs

{--
dropWhile                  :: (a -> Bool) -> [a] -> [a]
dropWhile p []                     = []
dropWhile p xs@(x:xs') | p x       = dropWhile p xs'
                       | otherwise = xs
--}

dropWhile                  :: (a -> Bool) -> [a] -> [a]
dropWhile p []                     = []
dropWhile p xs@(x:xs') = if p x then dropWhile p xs' else xs

infix 4 `elem`

elem :: (Eq a) => a -> [a] -> Bool
elem x = any (x==)

filter          :: (a -> Bool) -> [a] -> [a]
filter p []     =  []
filter p (x:xs) | p x       = x: filter p xs
                | otherwise = filter p xs

foldl        :: (a -> b -> a) -> a -> [b] -> a
foldl f z []     = z
foldl f z (x:xs) = foldl f (f z x) xs

foldl1        :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)   = foldl f x xs
foldl1 f []       = error "PreludeList.foldl1: empty list"

foldr        :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

foldr1       :: (a -> a -> a) -> [a] -> a
foldr1 f [x]     = x
foldr1 f (x:xs)  = f x (foldr1 f xs)
foldr1 _ []  = error "PreludeList.foldr1: empty list"

head        :: [a] -> a
head (x:_)  = x
head []     = error "PreludeList.head: empty list"

infixl 9 !!

(!!) :: [a] -> Int -> a

[]     !! _ = error "PreludeList.!!: on empty list"
(x:_)  !! 0 = x
(_:xs) !! n =
  if n < 0 then
    error "Prelude.!!: negative index"
  else
    xs `walk` (n-1)
 where
   walk           :: [a] -> Int -> a
   [] `walk` _     = error "Prelude.!!: index too large"
   (x:xs) `walk` 0 = x
   (_:xs) `walk` n = xs `walk` (n-1)

init        :: [a] -> [a]
init [x]    = []
init (x:xs)     = x : init xs
init []     = error "PreludeList.init: empty list"

iterate     :: (a -> a) -> a -> [a]
iterate f x     = x : iterate f (f x)

last        :: [a] -> a
last [x]    = x
last (_:xs)     = last xs
last []     = error "PreludeList.last: empty list"

length      :: [a] -> Int
length xs = ll 0 xs
  where
    ll :: Int -> [a] -> Int
    ll a [] = a
    ll a (_:xs) = let a1 = a+1 in a1 `seq` ll a1 xs

lines       :: String -> [String]
lines ""    = []
lines s = let (l,s') = break (== '\n') s
      in l : case s' of
              []      -> []
              (_:s'') -> lines s''

lookup :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup key [] = Nothing
lookup key ((x,y):xys)
     | key == x  = Just y
     | True  = lookup key xys

map          :: (a -> b) -> [a] -> [b]
map f []      =  []
map f (x:xs)  =  f x : map f xs

maximum :: (Ord a) => [a] -> a
maximum [] = error "PreludeList.maximum: empty list"
maximum xs = foldl1 max xs

minimum :: (Ord a) => [a] -> a
minimum [] = error "PreludeList.minimum: empty list"
minimum xs = foldl1 min xs

infix 4 `notElem`

notElem :: (Eq a) => a -> [a] -> Bool
notElem x = all (x/=)

null        :: [a] -> Bool
null []     = True
null (_:_)      = False

or      :: [Bool] -> Bool
or      = foldr (||) False

product :: (Num a) => [a] -> a
product = foldl (*) 1

repeat      :: a -> [a]
repeat x    = xs where xs = x:xs

replicate   :: Int -> a -> [a]
replicate n x   = take n (repeat x)

reverse     :: [a] -> [a]
reverse     = foldl (flip (:)) []

scanl        :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xs     = q : (case xs of
            []     -> []
            (x:xs) -> scanl f (f q x) xs)

scanl1       :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs)  = scanl f x xs
scanl1 f []      = []

scanr         :: (a -> b -> b) -> b -> [a] -> [b]
scanr f q0 []     = [q0]
scanr f q0 (x:xs) = f x q : qs
            where qs@(q:_) = scanr f q0 xs

scanr1      :: (a -> a -> a) -> [a] -> [a]
scanr1 f [x]    = [x]
scanr1 f (x:xs) = f x q : qs
          where qs@(q:_) = scanr1 f xs
scanr1 _ []     = []


span                      :: (a -> Bool) -> [a] -> ([a], [a])
span p []                 = ([], [])
span p xs@(x:xs') | p x       = let (ys, zs) = span p xs' in (x:ys, zs)
          | otherwise = ([], xs)


splitAt              :: Int -> [a] -> ([a], [a])
splitAt n xs | n <= 0 = ([],xs)
splitAt _ []          = ([],[])
splitAt n (x:xs)      = (x:xs',xs'') where (xs',xs'') = splitAt (n-1) xs


sum :: (Num a) => [a] -> a
sum = foldl (+) 0

tail        :: [a] -> [a]
tail (_:xs)     = xs
tail []     = error "PreludeList.tail: empty list"

take              :: Int -> [a] -> [a]
take n _ | n<=0        = []
take _ []              = []
take n (x:xs)          = x : take (n-1) xs

takeWhile              :: (a -> Bool) -> [a] -> [a]
takeWhile p []                 = []
takeWhile p (x:xs) | p x       = x : takeWhile p xs
           | otherwise = []

unlines     :: [String] -> String
unlines     = concatMap (++ "\n")

unwords     :: [String] -> String
unwords []  = ""
unwords ws  = foldr1 (\w s -> w ++ ' ':s) ws

unzip    :: [(a,b)] -> ([a],[b])
unzip    = foldr (\(b,c) ~(bs,cs) -> (b:bs, c:cs)) ([], [])

unzip3    :: [(a,b,c)] -> ([a],[b],[c])
unzip3    = foldr (\(b,c,d) ~(bs,cs,ds) -> (b:bs,c:cs,d:ds)) ([],[],[])

words       :: String -> [String]
words s     = case dropWhile isSpace s of
                  [] -> []
                  (s:ss) -> (s:w) : drop1 sss
                      where
                          (w, sss) = break isSpace ss

                          drop1 [] = []
                          drop1 (x:xs) = words xs


zip :: [a] -> [b] -> [(a,b)]
zip = zipWith (,)

zip3       :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3       = zipWith3 (,,)

zipWith                  :: (a->b->c) -> [a]->[b]->[c]
zipWith z (b:bs) (c:cs)   = z b c : zipWith z bs cs
zipWith _ _      _        = []

zipWith3                 :: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3 z (b:bs) (c:cs) (d:ds)
                          = z b c d : zipWith3 z bs cs ds
zipWith3 _ _ _ _          = []


----------------------------------------------------------------------------------------------------
-- Int
----------------------------------------------------------------------------------------------------

instance Bounded Int where
  minBound = negate 2147483648
  maxBound =        2147483647

instance Eq Int where
  a == b = a == b    --- MAGIC
  a /= b = a /= b    --- MAGIC

instance Ord Int where
  a <  b = a <  b -- MAGIC
  a <= b = a <= b -- MAGIC
  a >= b = a >= b -- MAGIC
  a >  b = a >  b -- MAGIC

instance Num Int where
 a + b    = a + b   -- MAGIC
 a - b    = a - b   -- MAGIC
 a * b    = a * b   -- MAGIC

 negate a = negate a    -- MAGIC
 abs    a = primIntAbs a   -- MAGIC
 signum a = primIntSignum a    -- MAGIC

 fromInteger i = primIntFromInteger i

instance Enum Int where
  toEnum = id
  fromEnum = id

  enumFrom n = n : enumFrom (n+1)
  enumFromThen m n = m : enumFromThen n (2*n-m)

instance Real Int where
  toRational i = (toInteger i) % 1

instance Integral Int  where
    n `quot`    d       = n `quot` d            -- MAGIC
    n `rem`     d       = n `rem`  d            -- MAGIC
    n `quotRem` d   = (n `quot` d, n `rem` d)   -- er, not so MAGIC!


    n `mod`  d = if signum r == negate (signum d) then (r+d) else r
                 where r = n `rem` d

    toInteger n     = primIntegerFromInt n

instance Show Int where
 -- We don't want to use the generic showsPrec and showInt, as they use Integer
 --  showsPrec = showSigned showInt
  showsPrec p x =
    if x < 0 then showParen (p > 6)
      (showChar '-' . if x == minBound
                      then showString "2147483648" -- WARNING 32bit
                      else showPosInt (negate x))
    else
      showPosInt x
   where
     showPosInt :: Int -> String -> String
     showPosInt n r =
      let
          quot10,rem10 :: Int -> Int   -- makes it easier for nhc98 to use byte code instructions
          quot10 n = n `quot` 10
          rem10 n = n `rem` 10
          nr :: Int -> Char      -- the magic constant 48 is fromEnum '0'
          nr d = toEnum (48 + d) -- nhc98 can only remove toEnum if it's in a strict context
          n' = quot10 n
          d' = rem10 n
          r' :: [Char]
          r' = nr d' : r
      in if n' == 0 then r' else showPosInt n' r'

  showsType a = showString "Int"

instance Read Int where
  readsPrec p r = map (\(i,t)-> (fromInteger i, t)) (readsPrec p r)

----------------------------------------------------------------------------------------------------
-- Float
----------------------------------------------------------------------------------------------------

instance Enum Float where
  succ x = x+1
  pred x = x-1

  toEnum = fromIntegral
  fromEnum = fromInteger . truncate

  enumFrom n          = iterate (+1) n
  enumFromTo n m
          | m<n       = []
          | otherwise = takeWhile (<=top) (iterate (+1) n)
      where top = m + 1/2
  enumFromThen n m    = iterate (+interval) n
      where interval  = m-n
  enumFromThenTo n m o
          | interval<0  && o>n = []
          | interval>0  && o<n = []
          | interval>=0        = takeWhile (<=top) (iterate (+interval) n)
          | interval<0         = takeWhile (>top) (iterate (+interval) n)
      where interval = m-n
            top = o + interval/2

instance Eq Float where
  a == b = a == b    --- MAGIC
  a /= b = a /= b    --- MAGIC

instance  Floating Float where
    pi          =  3.1415926535897932 -- 384626433832795028841972 -- Enough decimals
    exp x       =  primFloatExp x    -- MAGIC
    log x       =  primFloatLog x    -- MAGIC
    sqrt x      =  primFloatSqrt x   -- MAGIC
    sin x       =  primFloatSin x    -- MAGIC
    cos x       =  primFloatCos x    -- MAGIC
    tan x       =  primFloatTan x    -- MAGIC
    asin x      =  primFloatASin x   -- MAGIC
    acos x      =  primFloatACos x   -- MAGIC
    atan x      =  primFloatATan x   -- MAGIC
    x ** y      =  primFloatPow x y   -- MAGIC
    sinh x              = 0.5  * (primFloatExp x - primFloatExp (-x))
    cosh x              = 0.5  * (primFloatExp x + primFloatExp (-x))
    tanh x              = (af-bf)/(af+bf) where af = primFloatExp x ; bf = primFloatExp (-x)
    asinh x = primFloatLog (x + sqrt (1+x*x))
    acosh x = primFloatLog (x + (x+1) * sqrt ((x-1)/(x+1)))
    atanh x = primFloatLog ((x+1) / sqrt (1 - x*x))

instance  Fractional Float  where
  x / y =  x / y        -- MAGIC

  fromRational x = _floatFromRational x -- see PreludeAux.hs


instance Num Float where
 a + b    = a + b   -- MAGIC
 a - b    = a - b   -- MAGIC
 a * b    = a * b   -- MAGIC

 negate a = negate a    -- MAGIC
 abs    a = primFloatAbs a    -- MAGIC
 signum a = primFloatSignum a    -- MAGIC

 fromInteger i = primFloatFromInteger i

instance Ord Float where
  a <  b = a <  b -- MAGIC
  a <= b = a <= b -- MAGIC
  a >= b = a >= b -- MAGIC
  a >  b = a >  b -- MAGIC

instance  RealFrac Float where
    properFraction x =  case decodeFloat x of
              (m,n) ->
                if n >= 0 then
                (fromInteger m * fromInteger (floatRadix x) ^ n, 0)
                else
                case quotRem m ((floatRadix x)^(negate n)) of
                      (w,r) -> (fromInteger w, encodeFloat r n)

instance  RealFloat Float  where
    floatRadix _  = 2          -- FLT_RADIX
    floatDigits _ = 24         -- FLT_DIGITS
    floatRange _  = (negate 148,128) -- (FLT_MINEXP, FLT_MAXEXP)
    decodeFloat x = primDecodeFloat x
    encodeFloat x y = primEncodeFloat x y

    -- TODO
    isNaN x          = False
    isInfinite x     = False
    isDenormalized x = False
    isNegativeZero x = False
    isIEEE x         = False

instance  Real Float where
    toRational x = _floatToRational x -- see PreludeAux.hs

instance Show Float where
  showsPrec p = showFloat
  showsType a = showString "Float"

instance Read Float where
  readsPrec p = readSigned readFloat

----------------------------------------------------------------------------------------------------
-- Double
----------------------------------------------------------------------------------------------------

instance Enum Double where
  succ x = x+1
  pred x = x-1

  toEnum = fromIntegral
  fromEnum = fromInteger . truncate

  enumFrom n          = iterate (+1) n
  enumFromTo n m
          | m<n       = []
          | otherwise = takeWhile (<=top) (iterate (+1) n)
      where top = m + 1/2
  enumFromThen n m    = iterate (+interval) n
      where interval  = m-n
  enumFromThenTo n m o
          | interval<0  && o>n = []
          | interval>0  && o<n = []
          | interval>=0        = takeWhile (<=top) (iterate (+interval) n)
          | interval<0         = takeWhile (>top) (iterate (+interval) n)
      where interval = m-n
            top = o + interval/2

instance Eq Double where
  a == b = a == b    --- MAGIC
  a /= b = a /= b    --- MAGIC

instance  Floating Double where
    pi                  =  3.1415926535897932384626433832795028841972
    exp x               =  primDoubleExp x        -- MAGIC
    log x               =  primDoubleLog x        -- MAGIC
    sqrt x              =  primDoubleSqrt x       -- MAGIC
    sin x               =  primDoubleSin x        -- MAGIC
    cos x               =  primDoubleCos x        -- MAGIC
    tan x               =  primDoubleTan x        -- MAGIC
    asin x              =  primDoubleASin x       -- MAGIC
    acos x              =  primDoubleACos x       -- MAGIC
    atan x              =  primDoubleATan x       -- MAGIC
    x ** y              =  primDoublePow x y       -- MAGIC
    sinh x              = fromRational (1%2) * (primDoubleExp x - primDoubleExp (-x))
    cosh x              = fromRational (1%2) * (primDoubleExp x + primDoubleExp (-x))
    tanh x              = (a-b)/(a+b) where a = primDoubleExp x ; b = primDoubleExp (-x)
    asinh x = primDoubleLog (x + sqrt (1+x*x))
    acosh x = primDoubleLog (x + (x+1) * sqrt ((x-1)/(x+1)))
    atanh x = primDoubleLog ((x+1) / sqrt (1 - x*x))

instance  Fractional Double  where
  x / y = x / y         -- MAGIC

  fromRational x = _doubleFromRational x -- see PreludeAux.hs


instance Num Double where

 a + b    = a + b       -- MAGIC
 a - b    = a - b       -- MAGIC
 a * b    = a * b       -- MAGIC

 negate a = negate a    -- MAGIC
 abs    a = primDoubleAbs    a    -- MAGIC
 signum a = primDoubleSignum a    -- MAGIC

 fromInteger i = primDoubleFromInteger i


instance Ord Double where
  a <  b = a <  b -- MAGIC
  a <= b = a <= b -- MAGIC
  a >= b = a >= b -- MAGIC
  a >  b = a >  b -- MAGIC

instance  RealFloat Double  where
    floatRadix _  = 2          -- FLT_RADIX
    floatDigits _ = 53         -- DBL_DIGITS
    floatRange _  = (negate 1073,1024) -- (DBL_MINEXP, DBL_MAXEXP)
    decodeFloat x = primDecodeDouble x
    encodeFloat x y = primEncodeDouble x y

    -- TODO
    isNaN x          = False
    isInfinite x     = False
    isDenormalized x = False
    isNegativeZero x = False
    isIEEE x         = False

instance  RealFrac Double where
    properFraction x =  case decodeFloat x of
              (m,n) ->
                if n >= 0 then
                (fromInteger m * fromInteger (floatRadix x) ^ n, 0)
                else
                case quotRem m ((floatRadix x)^(negate n)) of
                      (w,r) -> (fromInteger w, encodeFloat r n)

instance  Real Double where
    toRational x = _doubleToRational x -- see PreludeAux.hs

instance Show Double where
  showsPrec p = showFloat
  showsType a = showString "Double"

instance Read Double where
  readsPrec p = readSigned readFloat

----------------------------------------------------------------------------------------------------
-- Integer
----------------------------------------------------------------------------------------------------

instance Enum Integer where
  succ x     = x+1
  pred x     = x-1
  toEnum x   = toInteger x
  fromEnum x = fromInteger x

  enumFrom x = x: enumFrom (x+1)
  enumFromThen x y = x : enumFromThen y (2*y-x)

instance Eq Integer where
  a == b = primIntegerEq a b
  a /= b = primIntegerNe a b

instance Ord Integer where
  a <  b = primIntegerLt a b
  a <= b = primIntegerLe a b
  a >= b = primIntegerGe a b
  a >  b = primIntegerGt a b

instance Num Integer where
 a + b    = primIntegerAdd a b
 a - b    = primIntegerSub a b
 a * b    = primIntegerMul a b
 negate a = primIntegerNeg a

 abs i = if i < 0 then negate i else i
 signum i = case compare i 0 of
        LT -> negate 1
        EQ ->  0
        GT ->  1
 fromInteger a = a -- id a

instance Real Integer where
  toRational i = i % 1

instance Integral Integer  where
    n `quot` d    = primIntegerQuot n d
    n `rem`  d    = primIntegerRem n d
    n `div`  d    = fst (divMod n d)
    n `mod`  d    = snd (divMod n d)

    n `quotRem` d = primIntegerQuotRem n d

    toInteger n   = n

instance Read Integer where
  readsPrec p = readSigned readDec

instance Show Integer where
  showsPrec = showSigned showInt
  showsType a = showString "Integer"

----------------------------------------------------------------------------------------------------
-- Bool
----------------------------------------------------------------------------------------------------

data Bool = True | False

instance Enum Bool where
    fromEnum   False = 0
    fromEnum   True  = 1

    toEnum   0 = False
    toEnum   1 = True
    toEnum   n = error ("(Prelude.toEnum "++show n++" :: Bool) is wrong")

    enumFrom n        = _enumFromTo  n 1
    enumFromThen n n' = _enumFromThen n n' 1

instance Eq Bool where
  True  == True  = True
  False == False = True
  _     == _     = False

instance Ord Bool where
  compare False False = EQ
  compare False True  = LT
  compare True  False = GT
  compare True  True  = EQ

instance Bounded Bool where
  maxBound = True
  minBound = False

instance Read Bool where
  readsPrec p = readParen False
                 ( \r -> [(False,s) | ("False",s) <- lex r] ++
                         [(True,s)  | ("True",s) <- lex r])

instance Show Bool where
  showsPrec p False = showString "False"
  showsPrec p True = showString "True"

  showsType a = showString "Bool"

infixr 3  &&

(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2  ||

(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

not :: Bool -> Bool
not True = False
not False = True

----------------------------------------------------------------------------------------------------
-- Char
----------------------------------------------------------------------------------------------------

instance Enum Char where
    enumFrom n        = enumFromTo n (toEnum 255)
    enumFromThen n n' = enumFromThenTo n n' (toEnum 255)

    toEnum   c = toEnum c   -- MAGIC
    fromEnum c = fromEnum c -- MAGIC

instance Eq Char where
  c == c' = fromEnum c == fromEnum c'

instance Ord Char where
  c <= c'      = fromEnum c <= fromEnum c'
  compare c c' = compare (fromEnum c) (fromEnum c')

instance Read Char where
    readsPrec p      = readParen False
                            (\r -> [(c,t) | ('\'':s,t)<- lex r,
                        (c,"\'")  <- readLitChar s])

    readList = readParen False (\r -> [(l,t) | ('"':s, t) <- lex r,
                           (l,_)      <- readl s    ])
           where
             readl ('"':s)  = [("",s)]
             readl ('\\':'&':s) = readl s
             readl s        = [(c:cs,u) | (c ,t) <- readLitChar s,
                              (cs,u) <- readl t       ]

instance Show Char where
    showsPrec p '\'' = showString "'\\''"
    showsPrec p c    = showChar '\'' . showLitChar c . showChar '\''

    showList cs = showChar '"' . showl cs
         where showl ""       = showChar '"'
               showl ('"':cs) = showString "\\\"" . showl cs
               showl (c:cs)   = showLitChar c . showl cs

    showsType x = showString "Char"

----------------------------------------------------------------------------------------------------
-- Unit
----------------------------------------------------------------------------------------------------

data ()                                             = ()

instance Eq () where
  a == b = True
  a /= b = False

instance Ord () where
  a <  b = False
  a <= b = True
  a >= b = True
  a >  b = False

----------------------------------------------------------------------------------------------------
-- Tuple
----------------------------------------------------------------------------------------------------

fst     :: (a,b) -> a
fst (x,y)   = x

snd     :: (a,b) -> b
snd (x,y)   = y

instance (Eq a,Eq b) => Eq (a,b) where
  (a,b) == (a',b') = a == a' && b == b'

instance (Ord a,Ord b) => Ord (a,b) where
  compare (a,b)  (a',b') =
    case compare a a' of
        LT -> LT
        EQ -> compare b b'
        GT -> GT

instance (Bounded a, Bounded b) => Bounded (a,b) where
  minBound = (minBound, minBound)
  maxBound = (maxBound, maxBound)

----------------------------------------------------------------------------------------------------
-- Triple
----------------------------------------------------------------------------------------------------

instance (Eq a,Eq b, Eq c) => Eq (a,b,c) where
  (a,b,c) == (a',b',c') = a == a' && b == b' && c == c'

instance (Ord a,Ord b,Ord c) => Ord (a,b,c) where
  compare (a,b,c)  (a',b',c') =
    case compare a a' of
        LT -> LT
        EQ -> case compare b b' of
                LT -> LT
                EQ -> compare c c'
                GT -> GT
        GT -> GT

instance (Bounded a, Bounded b, Bounded c) => Bounded (a,b,c) where
  minBound = (minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound)

----------------------------------------------------------------------------------------------------
-- Ordering
----------------------------------------------------------------------------------------------------

data Ordering = LT | EQ | GT

instance Enum Ordering where
    fromEnum LT = 0
    fromEnum EQ = 1
    fromEnum GT = 2

    toEnum   0 = LT
    toEnum   1 = EQ
    toEnum   2 = GT

    enumFrom n        = _enumFromTo n 2
    enumFromThen n n' = _enumFromThen n n' 2

instance Eq Ordering where
  a  == b = fromEnum a == fromEnum b

instance Ord Ordering where
  compare LT  LT = EQ
  compare LT  _  = LT
  compare EQ  LT = GT
  compare EQ  EQ = EQ
  compare EQ  GT = LT
  compare GT  GT = EQ
  compare EQ  _  = GT

instance Read Ordering where
  readsPrec p = readParen False
                 ( \r -> [(LT,s) | ("LT",s) <- lex r] ++
                         [(EQ,s) | ("EQ",s) <- lex r] ++
                         [(GT,s) | ("GT",s) <- lex r])

instance Show Ordering where
  showsPrec p LT = showString "LT"
  showsPrec p EQ = showString "EQ"
  showsPrec p GT = showString "GT"

  showsType a = showString "Ordering"

----------------------------------------------------------------------------------------------------
-- (a->b)
----------------------------------------------------------------------------------------------------

instance (Show a,Show b) => Show (a->b) where
  showsPrec d a = showString "<<function>>"

  showsType a = showChar '(' . showsType value  . showString " -> " .
                               showsType result . showChar ')'
                where (value,result) = getTypes undefined
                      getTypes x = (x,a x)

----------------------------------------------------------------------------------------------------
-- IO
----------------------------------------------------------------------------------------------------

_readCon :: a -> String -> ReadS a

_readCon con str =
    (\ r -> [(con,s) |
         (tok,s) <- lex r ,tok == str])

_readCon0 :: Bool -> a -> String -> ReadS a

_readCon0 b con str =
    readParen b (_readCon con str)

_readConArg :: (Read a) => (String -> [(a->b,String)]) -> ReadS b

_readConArg fun = \ r ->  [(c a,s) |
                   (c,r) <- fun r,
                   (a,s) <- readsPrec 10 r]

_readConInfix :: (Read a,Read b) => Int -> Int -> Int -> Int -> (a -> b -> c) -> String -> ReadS c

_readConInfix d p lp rp con str =
    readParen (d > p)
       (\ r -> [(con u v,s2) |
        (u,s0) <- readsPrec (lp+1) r,
        (tok,s1) <- lex s0, tok == str,
        (v,s2) <- readsPrec (rp+1) s1])

_readField :: (Read a) => String -> String -> (String -> [(a->b,String)]) -> ReadS b

_readField prefix name fun
    | (let h = head name in
       isLower h || h=='_') =               -- ordinary fieldname
        \ r ->  [(c a,s) | (c,r) <- fun r,
                   (tok,r) <- lex r,
                           tok == prefix,
                   (tok,r) <- lex r,
                           tok == name,
                   (tok,r) <- lex r,
                           tok == "=",
                   (a,s) <- readsPrec 0 r]
    | otherwise =                   -- symbol fieldname
        \ r ->  [(c a,s) | (c,r) <- fun r,
                   (tok,r) <- lex r,
                           tok == prefix,
                   (tok,r) <- lex r,
                           tok == "(",
                   (tok,r) <- lex r,
                           tok == init (tail name), -- trim parens off
                   (tok,r) <- lex r,
                           tok == ")",
                   (tok,r) <- lex r,
                           tok == "=",
                   (a,s) <- readsPrec 0 r]

_readFinal postfix reader =
        \ r ->  [(c,s) | (c,r) <- reader r,
                         (tok,s) <- lex r,
                         tok == postfix ]


appendFile :: FilePath -> String -> IO ()
appendFile fp str =
  openFile fp AppendMode >>= \ handle ->
  mapM_ (hPutChar handle) str >>
  hClose handle

getLine     :: IO String
getLine         =  do c <- getChar
                      if c == '\n' then return "" else
                         do s <- getLine
                            return (c:s)

interact :: (String -> String) -> IO ()
interact f = do s <- getContents
                putStr (f s)

getContents :: IO [Char]
getContents = hGetContents stdin

getChar :: IO Char
getChar =  hGetChar stdin

putChar :: Char -> IO ()
putChar c = hPutChar stdout c

putStr :: String -> IO ()
putStr = hPutStr stdout

putStrLn        :: String -> IO ()
putStrLn s      =  do putStr s
                      putChar '\n'

readIO :: Read a => String -> IO a
readIO s = case [x | (x,t) <- reads s, ("","") <- lex t] of
         [x] -> return x
         []  -> fail ("Prelude.readIO: no parse")
         _   -> fail ("Prelude.readIO: ambiguous parse")

readLn          :: Read a => IO a
readLn          =  do l <- getLine
                      r <- readIO l
                      return r

readFile :: FilePath -> IO String
readFile fp =
   openFile fp ReadMode >>= \ handle ->
   hGetContents handle

writeFile :: FilePath -> String -> IO ()
writeFile fp str =
  openFile fp WriteMode >>= \ handle ->
  mapM_ (hPutChar handle) str >>
  hClose handle

type FilePath  = String

data IOError
    --             operation  filename     file         error-code
    = IOError       String (Maybe String) (Maybe Handle) ErrNo
    --             operation  file
    | EOFError      String   Handle
    --             location
    | PatternError  String
    --             location  message
    | UserError     String   String

instance Eq IOError where
    (IOError ap af ah ae) == (IOError bp bf bh be)  = ap==bp && af==bf -- FIXUP: && ah==bh && ae == be
    (EOFError ap ah)      == (EOFError bp bh)       = ap==bp -- FIXUP: && ah==bh
    (PatternError al)     == (PatternError bl)      = al==bl
    (UserError al am)     == (UserError bl bm)      = al==bl && am==bm
    _                     == _                      = False

ioError :: Prelude.IOError -> IO a
ioError e = throwIO (IOException e)

userError :: String -> IOError
userError s = UserError "call to function `userError'" s

instance Monad IO where

    x >>= y  = IO (ioBind x y)
#ifndef __HADDOCK__
           where
           ioBind (IO xf) y w = let xe = xf w
                                in case xe of
                                     _E xv -> case y xv of
                                        IO yf -> yf w
#endif
    x >>  y  = IO (ioSeq x y)
           where
           ioSeq (IO xf) (IO yf) w =  xf w `seq` yf w

    return a = IO (ioReturn a)
#ifndef __HADDOCK__
            where
            ioReturn a w = _E a
#endif

catch :: IO a -> (Prelude.IOError -> IO a) -> IO a
catch x handle = catchException x $ \ e ->
        case e of
            IOException iox -> handle iox
            _               -> throwIO e

instance  Show IOError  where
        showsPrec p (IOError cmd mbfilename mbhandle errno)  =
                showString "I/O error:\n  action :  " .
                showString cmd .
                (case mbfilename of
                    Just fn -> showString "\n  on file:  " . showString fn
                    Nothing ->
                        (case mbhandle of
                            Nothing -> id
                            Just handle ->
                                (case hGetFileName handle of
                                    Nothing -> id
                                    Just fn ->
                                        showString "\n  on file:  " .
                                        showString fn))) .
                showString "\n  gave   :  " .
                _showsErrNo errno . showString " (" .
                showString (strError errno) .
                showString ")"
        showsPrec p (EOFError op handle) =
                showString "End of file detected in " .
                showString op . showString " on " .
                (case hGetFileName handle of
                            Nothing -> showString "un-named handle"
                            Just fn -> showString fn) . showChar '.'
        showsPrec p (PatternError "") =
                showString "Pattern match failure in do statement"
        showsPrec p (PatternError str) =
                showString "Pattern match failure in do statement (" .
                showString str . showString ")"
        showsPrec p (UserError "" str) =
                showString "I/O error (user-defined):\n  "  . showString str
        showsPrec p (UserError loc str) =
                showString "I/O error (user-defined), " . showString loc .
                showString ":\n  " . showString str

        showsType a = showString "IOError"


strError :: ErrNo -> String
strError e = unsafePerformIO $ peekCString (primStrError (_fromEnumErrNo e))

instance (Show a) => Show (IO a) where
  showsPrec d a = showString "<<IO action>>"

  showsType (IO fta) = showString "(IO " . showsType ta . showChar ')'
#ifndef __HADDOCK__
                       where (_E  ta) = fta World
#endif

----------------------------------------------------------------------------------------------------
-- Numeric
----------------------------------------------------------------------------------------------------

fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral = fromInteger . toInteger

numericEnumFrom :: (Fractional a) => a -> [a]
numericEnumFrom n = iterate (+1) n

numericEnumFromTo :: (Fractional a, Ord a) => a -> a -> [a]
numericEnumFromTo n m = takeWhile (<= m+1/2) (numericEnumFrom n)

numericEnumFromThen :: (Fractional a) => a-> a-> [a]
numericEnumFromThen n m = iterate (+(m-n)) n

numericEnumFromThenTo :: (Fractional a, Ord a) => a-> a-> a-> [a]
numericEnumFromThenTo n n' m = takeWhile p (numericEnumFromThen n n')
                             where
                               p | n' > n    = (<= m + (n'-n)/2)
                                 | otherwise = (>= m + (n'-n)/2)

realToFrac :: (Real a, Fractional b) => a -> b
realToFrac = fromRational . toRational

infixr 8  ^

(^)     :: (Num a, Integral b) => a -> b -> a

x ^ 0       = 1
x ^ n | n > 0   = f x (n-1) x
          where f _ 0 y = y
                f x n y = g x n
                    where  g x n | even n    = g (x*x) (n `quot` 2)
                                 | otherwise = f x (n-1) (x*y)
_ ^ _       = error "Prelude.(^): negative exponent"


infixr 8  ^^

(^^)        :: (Fractional a, Integral b) => a -> b -> a
x ^^ n      =  if n >= 0 then x^n else recip(x^(negate n))


even :: (Integral a) => a -> Bool
even n = n `rem` 2 == 0

odd :: (Integral a) => a -> Bool
odd = not . even

subtract :: (Num a) => a -> a -> a
subtract = flip (-)

----------------------------------------------------------------------------------------------------
-- Show
----------------------------------------------------------------------------------------------------

type ShowS  = String -> String

print :: Show a => a -> IO ()
print x = putStrLn (show x)

showString  :: String -> ShowS
showString  = (++)

showParen   :: Bool -> ShowS -> ShowS
showParen b p   = if b then showChar '(' . p . showChar ')' else p

showChar    :: Char -> ShowS
showChar        = (:)

showType                :: (Show a) => a -> String
showType x              =  showsType x ""

shows           :: (Show a) => a -> ShowS
shows       =  showsPrec 0

----------------------------------------------------------------------------------------------------
-- Read
----------------------------------------------------------------------------------------------------

type ReadS a = String -> [(a,String)]

lex                     :: ReadS String


lex ""                  = [("","")]
lex (c:s) | isSpace c   = lex (dropWhile isSpace s)
lex ('\'':s)            = [('\'':ch++"'", t) | (ch,'\'':t)  <- lexLitChar s,
                                               ch /= "'"                ]
lex ('"':s)             = [('"':str, t)      | (str,t) <- lexString s]
                          where
                          lexString ('"':s) = [("\"",s)]
                          lexString s = [(ch++str, u)
                                                | (ch,t)  <- lexStrItem s,
                                                  (str,u) <- lexString t  ]

                          lexStrItem ('\\':'&':s) = [("\\&",s)]
                          lexStrItem ('\\':c:s) | isSpace c
                              = [("\\&",t) | '\\':t <- [dropWhile isSpace s]]
                          lexStrItem s            = lexLitChar s

lex (c:s) | isSingle c  = [([c],s)]
          | isSym c     = [(c:sym,t)         | (sym,t) <- [span isSym s]]
          | isIdInit c  = [(c:nam,t)         | (nam,t) <- [span isIdChar s]]
          | isDigit c   = [(c:ds++fe,t)      | (ds,s)  <- [span isDigit s],
                                               (fe,t)  <- lexFracExp s     ]
          | otherwise   = []    -- bad character
                where
                isSingle c  =  c `elem` ",;()[]{}`"
                isSym c     =  c `elem` "!@#$%&*+./<=>?\\^|:-~"
                isIdInit c  =  isAlpha c || c == '_'
                isIdChar c  =  isAlphaNum c || c `elem` "_'"

                lexFracExp ('.':c:s) | isDigit c
                                   = [('.':ds++e,u) | (ds,t) <- lexDigits (c:s),
                                                      (e,u)  <- lexExp t    ]
                lexFracExp s       = lexExp s

                lexExp (e:s) | e `elem` "eE"
                         = [(e:c:ds,u) | (c:t)  <- [s], c `elem` "+-",
                                                   (ds,u) <- lexDigits t] ++
                           [(e:ds,t)   | (ds,t) <- lexDigits s]
                lexExp s = [("",s)]

read            :: (Read a) => String -> a
read s          =  case [x | (x,t) <- reads s, all isSpace t] of
            [x] -> x
            []  -> error "Prelude.read: no parse"
            _   -> error "Prelude.read: ambiguous parse"

readParen       :: Bool -> ReadS a -> ReadS a
readParen b g   =  if b then mandatory else optional
           where optional r  = g r ++ mandatory r
                 mandatory r = [(x,u) | ("(",s) <- lex r,
                               (x,t)   <- optional s,
                               (")",u) <- lex t    ]


reads           :: (Read a) => ReadS a
reads       =  readsPrec 0

----------------------------------------------------------------------------------------------------
-- Monad
----------------------------------------------------------------------------------------------------

mapM            :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f as       =  sequence (map f as)

mapM_           :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f as      =  sequence_ (map f as)

sequence    :: Monad m => [m a] -> m [a]
sequence []     = return []
sequence (c:cs) = do x  <- c
                     xs <- sequence cs
                     return (x:xs)

sequence_   :: Monad m => [m a] -> m ()
sequence_   = foldr (>>) (return ())

infixr 1  =<<

(=<<)          :: Monad m => (a -> m b) -> m a -> m b
f =<< x         =  x >>= f
#ifndef __HADDOCK__
instance Monad ((->) a) where
#endif


----------------------------------------------------------------------------------------------------
-- Functions
----------------------------------------------------------------------------------------------------

infixr 0  $


($)     :: (a->b) -> a -> b
f $ x   = f x

infixr 0 $!


($!)        :: (a -> b) -> a -> b
f $! x       =  x `seq` f x


flip        :: (a -> b -> c) -> b -> a -> c
flip f x y  = f y x

id      :: a -> a
id x        = x

const       :: a -> b -> a
const x _   = x

otherwise :: Bool
otherwise = True

error :: String -> a
error s = throw (ErrorCall s)

asTypeOf :: a -> a -> a
asTypeOf = const

undefined :: a
undefined = error "Prelude.undefined"

gcd     :: (Integral a) => a -> a -> a
gcd 0 0 = error "Prelude.gcd: gcd 0 0 is undefined."
gcd x y = gcd' (abs x) (abs y)
          where gcd' :: (Integral a) => a -> a -> a
                gcd' x 0 = x
                gcd' x y = gcd' y (x `rem` y)

lcm     :: (Integral a) => a -> a-> a
lcm _ 0     =  0
lcm 0 _     =  0
lcm x y     =  abs ((x `quot` (gcd x y)) * y)


infixr 9 .

(.)  :: (b -> c) -> (a -> b) -> a -> c
f . g = \ x -> f (g x)

seq         :: a -> b -> b
seq a b      = _seq a b      -- MAGIC (converted to a single bytecode)


until      :: (a -> Bool) -> (a -> a) -> a -> a
until p f x = if p x
          then x
              else until p f (f x)


curry       :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry     :: (a -> b -> c) -> (a,b) -> c
uncurry f p = f (fst p) (snd p)

----------------------------------------------------------------------------------------------------
-- Magic!
----------------------------------------------------------------------------------------------------
_seq        :: a -> b -> b
_seq a b     = _seq a b

_filter :: Bool -> ([a]->[a]) -> [a] -> [a]
_filter b e r = if b then e r else r

_foldr :: (a -> b -> b) -> [a] -> b -> b
_foldr f [] d = d
_foldr f ((:) x xs) d = f x (_foldr f xs d)

_fromEnum  :: a -> Int
_fromEnum a = _fromEnum a -- MAGIC

_toEnum  :: Int -> a
_toEnum a = _toEnum a -- MAGIC

_enumFromTo :: a -> Int -> [a]
_enumFromTo n m = _enumFromTo' (_fromEnum n) m

_enumFromTo' :: Int -> Int -> [a]
_enumFromTo' n m =
  case compare n m of
    LT -> _toEnum n : _enumFromTo' (n+1) m
    EQ -> [_toEnum n]
    GT -> []

_enumFromThenTo :: a -> a -> Int -> [a]
_enumFromThenTo n n' m =
   let estep = _fromEnum n' - _fromEnum n
   in if estep >= 0 then
      _enumFromToInc (_fromEnum n) estep m
      else
      _enumFromToDec (_fromEnum n) estep m

_enumFromThen :: a -> a -> Int -> [a]
_enumFromThen n n' m =
   let step = _fromEnum n' - _fromEnum n
   in if step >= 0 then
      _enumFromToInc (_fromEnum n) step m
      else
      _enumFromToDec (_fromEnum n) step 0

_enumFromToDec :: Int -> Int -> Int -> [a]
_enumFromToDec n s m =
  case compare n m of
    LT -> []
    EQ -> [_toEnum n]
    GT -> _toEnum n : _enumFromToDec (n+s) s m

_enumFromToInc :: Int -> Int -> Int -> [a]
_enumFromToInc n s m =
  case compare n m of
    LT -> _toEnum n : _enumFromToInc (n+s) s m
    EQ -> [_toEnum n]
    GT -> []

_enumIndex :: String -> (a,a) -> a -> Int
_enumIndex msg b@(c,c') ci
   | _enumInRange b ci = _fromEnum ci - _fromEnum c
   | True              = error ("Ix." ++ msg ++ ".index: Index out of range.")

_enumInRange :: (a,a) -> a -> Bool
_enumInRange (c,c') ci =    _fromEnum c  <= _fromEnum ci
                         && _fromEnum ci <= _fromEnum c'

_enumRange :: (a,a) -> [a]
_enumRange (n,m) = _enumFromTo' (_fromEnum n) (_fromEnum m)

_leInteger a b   = primIntegerLe a b
_subInteger a b  = primIntegerSub a b

_zap_arg = error "Evaluating zapped argument"
_zap_stack = error "Evaluating zapped part of the stack"

_black_hole = _nonTermination

_blockedOnDeadMVar = throw BlockedOnDeadMVar
_deadlock = throw Deadlock
_divByZero = throw (ArithException DivideByZero)
_noMethodError s = throw (NoMethodError s)
_nonTermination = throw NonTermination
_patternMatchFail s = throw (PatternMatchFail s)
_recConError s = throw (RecConError s)
_recSelError s = throw (RecSelError s)
_recUpdError s = throw (RecUpdError s)

------------------------------------------------------------------------------------------------------------
-- Show Tuples
------------------------------------------------------------------------------------------------------------

instance Show () where
    showsPrec p () = showString "()"

    showsType a = showString "()"

instance  (Show a, Show b) => Show (a,b)  where
    showsPrec p (x,y) = showChar '(' . shows x . showString "," .
                                       shows y . showChar ')'

    showsType  ~(x,y) = showChar '(' . showsType x . showChar ',' .
                                       showsType y . showChar ')'


instance  (Show a, Show b, Show c) => Show (a,b,c)  where
    showsPrec p (x,y,z) = showChar '(' . shows x . showString "," .
                                         shows y . showString "," .
                     shows z . showChar ')'

    showsType  ~(x,y,z) = showChar '(' . showsType x . showChar ',' .
                                         showsType y . showChar ',' .
                     showsType z . showChar ')'


instance  (Show a, Show b, Show c, Show d) => Show (a,b,c,d)  where
    showsPrec p (x,y,z,u) = showChar '(' . shows x . showString "," .
                                           shows y . showString "," .
                                           shows z . showString "," .
                       shows u . showChar ')'

    showsType  ~(x,y,z,u) = showChar '(' . showsType x . showChar ',' .
                                           showsType y . showChar ',' .
                       showsType z . showChar ',' .
                       showsType u . showChar ')'

instance  (Show a, Show b, Show c, Show d, Show e) => Show (a,b,c,d,e)  where
    showsPrec p (x,y,z,u,v) = showChar '(' . shows x . showString "," .
                                           shows y . showString "," .
                                           shows z . showString "," .
                       shows u . showString "," .
                       shows v . showChar ')'

    showsType  ~(x,y,z,u,v) = showChar '(' . showsType x . showChar ',' .
                                           showsType y . showChar ',' .
                       showsType z . showChar ',' .
                       showsType u . showChar ',' .
                       showsType v . showChar ')'

instance  (Show a, Show b, Show c, Show d, Show e, Show f) =>
     Show (a,b,c,d,e,f)  where
    showsPrec p (x,y,z,u,v,w) = showChar '(' . shows x . showString "," .
                                           shows y . showString "," .
                                           shows z . showString "," .
                       shows u . showString "," .
                       shows v . showString "," .
                       shows w . showChar ')'

    showsType  ~(x,y,z,u,v,w) = showChar '(' . showsType x . showChar ',' .
                                           showsType y . showChar ',' .
                       showsType z . showChar ',' .
                       showsType u . showChar ',' .
                       showsType v . showChar ',' .
                       showsType w . showChar ')'

instance  (Show a, Show b, Show c, Show d, Show e, Show f, Show g) =>
     Show (a,b,c,d,e,f,g)  where
    showsPrec p (x,y,z,u,v,w,t) = showChar '(' . shows x . showString "," .
                                           shows y . showString "," .
                                           shows z . showString "," .
                       shows u . showString "," .
                       shows v . showString "," .
                       shows w . showString "," .
                       shows t . showChar ')'

    showsType  ~(x,y,z,u,v,w,t) = showChar '(' . showsType x . showChar ',' .
                                           showsType y . showChar ',' .
                       showsType z . showChar ',' .
                       showsType u . showChar ',' .
                       showsType v . showChar ',' .
                       showsType w . showChar ',' .
                       showsType t . showChar ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
      Show h) =>
     Show (a,b,c,d,e,f,g,h)  where
    showsPrec p (x,y,z,u,v,w,t,a) = showChar '(' . shows x . showString "," .
                                           shows y . showString "," .
                                           shows z . showString "," .
                       shows u . showString "," .
                       shows v . showString "," .
                       shows w . showString "," .
                       shows t . showString "," .
                       shows a . showChar ')'

    showsType  ~(x,y,z,u,v,w,t,a) = showChar '(' . showsType x . showChar ',' .
                                           showsType y . showChar ',' .
                       showsType z . showChar ',' .
                       showsType u . showChar ',' .
                       showsType v . showChar ',' .
                       showsType w . showChar ',' .
                       showsType t . showChar ',' .
                       showsType a . showChar ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
      Show h, Show i) =>
     Show (a,b,c,d,e,f,g,h,i)  where
    showsPrec p (x,y,z,u,v,w,t,a,b) =
             showChar '(' . shows x . showString "," .
                                           shows y . showString "," .
                                           shows z . showString "," .
                       shows u . showString "," .
                       shows v . showString "," .
                       shows w . showString "," .
                       shows t . showString "," .
                       shows a . showString "," .
                       shows b . showChar ')'

    showsType  ~(x,y,z,u,v,w,t,a,b) =
             showChar '(' . showsType x . showChar ',' .
                                           showsType y . showChar ',' .
                       showsType z . showChar ',' .
                       showsType u . showChar ',' .
                       showsType v . showChar ',' .
                       showsType w . showChar ',' .
                       showsType t . showChar ',' .
                       showsType a . showChar ',' .
                       showsType b . showChar ')'


instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
      Show h, Show i, Show j) =>
     Show (a,b,c,d,e,f,g,h,i,j)  where
    showsPrec p (x,y,z,u,v,w,t,a,b,c) =
             showChar '(' . shows x . showString "," .
                                           shows y . showString "," .
                                           shows z . showString "," .
                       shows u . showString "," .
                       shows v . showString "," .
                       shows w . showString "," .
                       shows t . showString "," .
                       shows a . showString "," .
                       shows b . showString "," .
                       shows c . showChar ')'

    showsType  ~(x,y,z,u,v,w,t,a,b,c) =
             showChar '(' . showsType x . showChar ',' .
                                           showsType y . showChar ',' .
                       showsType z . showChar ',' .
                       showsType u . showChar ',' .
                       showsType v . showChar ',' .
                       showsType w . showChar ',' .
                       showsType t . showChar ',' .
                       showsType a . showChar ',' .
                       showsType b . showChar ',' .
                       showsType c . showChar ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
      Show h, Show i, Show j, Show k) =>
     Show (a,b,c,d,e,f,g,h,i,j,k)  where
    showsPrec p (x,y,z,u,v,w,t,a,b,c,d) =
             showChar '(' . shows x . showString "," .
                                           shows y . showString "," .
                                           shows z . showString "," .
                       shows u . showString "," .
                       shows v . showString "," .
                       shows w . showString "," .
                       shows t . showString "," .
                       shows a . showString "," .
                       shows b . showString "," .
                       shows c . showString "," .
                       shows d . showChar ')'

    showsType  ~(x,y,z,u,v,w,t,a,b,c,d) =
             showChar '(' . showsType x . showChar ',' .
                                           showsType y . showChar ',' .
                       showsType z . showChar ',' .
                       showsType u . showChar ',' .
                       showsType v . showChar ',' .
                       showsType w . showChar ',' .
                       showsType t . showChar ',' .
                       showsType a . showChar ',' .
                       showsType b . showChar ',' .
                       showsType c . showChar ',' .
                       showsType d . showChar ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
      Show h, Show i, Show j, Show k, Show l) =>
     Show (a,b,c,d,e,f,g,h,i,j,k,l)  where
    showsPrec p (x,y,z,u,v,w,t,a,b,c,d,e) =
             showChar '(' . shows x . showString "," .
                                           shows y . showString "," .
                                           shows z . showString "," .
                       shows u . showString "," .
                       shows v . showString "," .
                       shows w . showString "," .
                       shows t . showString "," .
                       shows a . showString "," .
                       shows b . showString "," .
                       shows c . showString "," .
                       shows d . showString "," .
                       shows e . showChar ')'

    showsType  ~(x,y,z,u,v,w,t,a,b,c,d,e) =
             showChar '(' . showsType x . showChar ',' .
                                           showsType y . showChar ',' .
                       showsType z . showChar ',' .
                       showsType u . showChar ',' .
                       showsType v . showChar ',' .
                       showsType w . showChar ',' .
                       showsType t . showChar ',' .
                       showsType a . showChar ',' .
                       showsType b . showChar ',' .
                       showsType c . showChar ',' .
                       showsType d . showChar ',' .
                       showsType e . showChar ')'


instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
      Show h, Show i, Show j, Show k, Show l, Show m) =>
     Show (a,b,c,d,e,f,g,h,i,j,k,l,m)  where
    showsPrec p (x,y,z,u,v,w,t,a,b,c,d,e,f) =
             showChar '(' . shows x . showString "," .
                                           shows y . showString "," .
                                           shows z . showString "," .
                       shows u . showString "," .
                       shows v . showString "," .
                       shows w . showString "," .
                       shows t . showString "," .
                       shows a . showString "," .
                       shows b . showString "," .
                       shows c . showString "," .
                       shows d . showString "," .
                       shows e . showString "," .
                       shows f . showChar ')'

    showsType  ~(x,y,z,u,v,w,t,a,b,c,d,e,f) =
             showChar '(' . showsType x . showChar ',' .
                                           showsType y . showChar ',' .
                       showsType z . showChar ',' .
                       showsType u . showChar ',' .
                       showsType v . showChar ',' .
                       showsType w . showChar ',' .
                       showsType t . showChar ',' .
                       showsType a . showChar ',' .
                       showsType b . showChar ',' .
                       showsType c . showChar ',' .
                       showsType d . showChar ',' .
                       showsType e . showChar ',' .
                       showsType f . showChar ')'



instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
      Show h, Show i, Show j, Show k, Show l, Show m, Show n) =>
     Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n)  where
    showsPrec p (x,y,z,u,v,w,t,a,b,c,d,e,f,g) =
             showChar '(' . shows x . showString "," .
                                           shows y . showString "," .
                                           shows z . showString "," .
                       shows u . showString "," .
                       shows v . showString "," .
                       shows w . showString "," .
                       shows t . showString "," .
                       shows a . showString "," .
                       shows b . showString "," .
                       shows c . showString "," .
                       shows d . showString "," .
                       shows e . showString "," .
                       shows f . showString "," .
                       shows g . showChar ')'

    showsType  ~(x,y,z,u,v,w,t,a,b,c,d,e,f,g) =
             showChar '(' . showsType x . showChar ',' .
                                           showsType y . showChar ',' .
                       showsType z . showChar ',' .
                       showsType u . showChar ',' .
                       showsType v . showChar ',' .
                       showsType w . showChar ',' .
                       showsType t . showChar ',' .
                       showsType a . showChar ',' .
                       showsType b . showChar ',' .
                       showsType c . showChar ',' .
                       showsType d . showChar ',' .
                       showsType e . showChar ',' .
                       showsType f . showChar ',' .
                       showsType g . showChar ')'


instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
      Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o) =>
     Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)  where
    showsPrec p (x,y,z,u,v,w,t,a,b,c,d,e,f,g,h) =
             showChar '(' . shows x . showString "," .
                                           shows y . showString "," .
                                           shows z . showString "," .
                       shows u . showString "," .
                       shows v . showString "," .
                       shows w . showString "," .
                       shows t . showString "," .
                       shows a . showString "," .
                       shows b . showString "," .
                       shows c . showString "," .
                       shows d . showString "," .
                       shows e . showString "," .
                       shows f . showString "," .
                       shows g . showString "," .
                       shows h . showChar ')'

    showsType  ~(x,y,z,u,v,w,t,a,b,c,d,e,f,g,h) =
             showChar '(' . showsType x . showChar ',' .
                                           showsType y . showChar ',' .
                       showsType z . showChar ',' .
                       showsType u . showChar ',' .
                       showsType v . showChar ',' .
                       showsType w . showChar ',' .
                       showsType t . showChar ',' .
                       showsType a . showChar ',' .
                       showsType b . showChar ',' .
                       showsType c . showChar ',' .
                       showsType d . showChar ',' .
                       showsType e . showChar ',' .
                       showsType f . showChar ',' .
                       showsType g . showChar ',' .
                       showsType h . showChar ')'


------------------------------------------------------------------------------------------------------------
-- Read Tuples
------------------------------------------------------------------------------------------------------------

instance Read () where
  readsPrec p = readParen False
                 ( \r -> [((),t)  | ("(",s) <- lex r
                                  , (")",t) <- lex s] )

instance  (Read a, Read b) => Read (a,b)  where
    readsPrec p = readParen False
                            (\r -> [((x,y), w) | ("(",s) <- lex r,
                                                 (x,t)   <- reads s,
                                                 (",",u) <- lex t,
                                                 (y,v)   <- reads u,
                                                 (")",w) <- lex v ] )

instance  (Read a, Read b, Read c) => Read (a,b,c)  where
    readsPrec p = readParen False
                            (\r0 -> [((x1,x2,x3), w) |
                                        ("(",s1) <- lex r0,
                                        (x1, r1) <- reads s1,
                                        (",",s2) <- lex r1,
                                        (x2, r2) <- reads s2,
                                        (",",s3) <- lex r2,
                                        (x3, r3) <- reads s3,
                                        (")",w)  <- lex r3 ] )

instance  (Read a, Read b, Read c, Read d) => Read (a,b,c,d)  where
    readsPrec p = readParen False
                            (\r0 -> [((x1,x2,x3,x4), w) |
                                        ("(",s1) <- lex r0,
                                        (x1, r1) <- reads s1,
                                        (",",s2) <- lex r1,
                                        (x2, r2) <- reads s2,
                                        (",",s3) <- lex r2,
                                        (x3, r3) <- reads s3,
                                        (",",s4) <- lex r3,
                                        (x4, r4) <- reads s4,
                                        (")",w)  <- lex r4 ] )

instance  (Read a, Read b, Read c, Read d, Read e) => Read (a,b,c,d,e)  where
    readsPrec p = readParen False
                            (\r0 -> [((x1,x2,x3,x4,x5), w) |
                                        ("(",s1) <- lex r0,
                                        (x1, r1) <- reads s1,
                                        (",",s2) <- lex r1,
                                        (x2, r2) <- reads s2,
                                        (",",s3) <- lex r2,
                                        (x3, r3) <- reads s3,
                                        (",",s4) <- lex r3,
                                        (x4, r4) <- reads s4,
                                        (",",s5) <- lex r4,
                                        (x5, r5) <- reads s5,
                                        (")",w)  <- lex r5 ] )

instance  (Read a, Read b, Read c, Read d, Read e, Read f) =>
         Read (a,b,c,d,e,f)  where
    readsPrec p = readParen False
                            (\r0 -> [((x1,x2,x3,x4,x5,x6), w) |
                                        ("(",s1) <- lex r0,
                                        (x1, r1) <- reads s1,
                                        (",",s2) <- lex r1,
                                        (x2, r2) <- reads s2,
                                        (",",s3) <- lex r2,
                                        (x3, r3) <- reads s3,
                                        (",",s4) <- lex r3,
                                        (x4, r4) <- reads s4,
                                        (",",s5) <- lex r4,
                                        (x5, r5) <- reads s5,
                                        (",",s6) <- lex r5,
                                        (x6, r6) <- reads s6,
                                        (")",w)  <- lex r6 ] )

instance  (Read a, Read b, Read c, Read d, Read e, Read f, Read g) =>
         Read (a,b,c,d,e,f,g)  where
    readsPrec p = readParen False
                            (\r0 -> [((x1,x2,x3,x4,x5,x6,x7), w) |
                                        ("(",s1) <- lex r0,
                                        (x1, r1) <- reads s1,
                                        (",",s2) <- lex r1,
                                        (x2, r2) <- reads s2,
                                        (",",s3) <- lex r2,
                                        (x3, r3) <- reads s3,
                                        (",",s4) <- lex r3,
                                        (x4, r4) <- reads s4,
                                        (",",s5) <- lex r4,
                                        (x5, r5) <- reads s5,
                                        (",",s6) <- lex r5,
                                        (x6, r6) <- reads s6,
                                        (",",s7) <- lex r6,
                                        (x7, r7) <- reads s7,
                                        (")",w)  <- lex r7 ] )

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g,
          Read h) =>
         Read (a,b,c,d,e,f,g,h)  where
    readsPrec p = readParen False
                            (\r0 -> [((x1,x2,x3,x4,x5,x6,x7,x8), w) |
                                        ("(",s1) <- lex r0,
                                        (x1, r1) <- reads s1,
                                        (",",s2) <- lex r1,
                                        (x2, r2) <- reads s2,
                                        (",",s3) <- lex r2,
                                        (x3, r3) <- reads s3,
                                        (",",s4) <- lex r3,
                                        (x4, r4) <- reads s4,
                                        (",",s5) <- lex r4,
                                        (x5, r5) <- reads s5,
                                        (",",s6) <- lex r5,
                                        (x6, r6) <- reads s6,
                                        (",",s7) <- lex r6,
                                        (x7, r7) <- reads s7,
                                        (",",s8) <- lex r7,
                                        (x8, r8) <- reads s8,
                                        (")",w)  <- lex r8 ] )

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g,
          Read h, Read i) =>
         Read (a,b,c,d,e,f,g,h,i)  where
    readsPrec p = readParen False
                            (\r0 -> [((x1,x2,x3,x4,x5,x6,x7,x8,x9), w) |
                                        ("(",s1) <- lex r0,
                                        (x1, r1) <- reads s1,
                                        (",",s2) <- lex r1,
                                        (x2, r2) <- reads s2,
                                        (",",s3) <- lex r2,
                                        (x3, r3) <- reads s3,
                                        (",",s4) <- lex r3,
                                        (x4, r4) <- reads s4,
                                        (",",s5) <- lex r4,
                                        (x5, r5) <- reads s5,
                                        (",",s6) <- lex r5,
                                        (x6, r6) <- reads s6,
                                        (",",s7) <- lex r6,
                                        (x7, r7) <- reads s7,
                                        (",",s8) <- lex r7,
                                        (x8, r8) <- reads s8,
                                        (",",s9) <- lex r8,
                                        (x9, r9) <- reads s9,
                                        (")",w)  <- lex r9 ] )

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g,
          Read h, Read i, Read j) =>
         Read (a,b,c,d,e,f,g,h,i,j)  where
    readsPrec p = readParen False
                            (\r0 -> [((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10), w) |
                                        ("(",s1) <- lex r0,
                                        (x1, r1) <- reads s1,
                                        (",",s2) <- lex r1,
                                        (x2, r2) <- reads s2,
                                        (",",s3) <- lex r2,
                                        (x3, r3) <- reads s3,
                                        (",",s4) <- lex r3,
                                        (x4, r4) <- reads s4,
                                        (",",s5) <- lex r4,
                                        (x5, r5) <- reads s5,
                                        (",",s6) <- lex r5,
                                        (x6, r6) <- reads s6,
                                        (",",s7) <- lex r6,
                                        (x7, r7) <- reads s7,
                                        (",",s8) <- lex r7,
                                        (x8, r8) <- reads s8,
                                        (",",s9) <- lex r8,
                                        (x9, r9) <- reads s9,
                                        (",",s10) <- lex r9,
                                        (x10, r10) <- reads s10,
                                        (")",w)  <- lex r10 ] )

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g,
          Read h, Read i, Read j, Read k) =>
         Read (a,b,c,d,e,f,g,h,i,j,k)  where
    readsPrec p = readParen False
                    (\r0 -> [((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11), w) |
                                        ("(",s1) <- lex r0,
                                        (x1, r1) <- reads s1,
                                        (",",s2) <- lex r1,
                                        (x2, r2) <- reads s2,
                                        (",",s3) <- lex r2,
                                        (x3, r3) <- reads s3,
                                        (",",s4) <- lex r3,
                                        (x4, r4) <- reads s4,
                                        (",",s5) <- lex r4,
                                        (x5, r5) <- reads s5,
                                        (",",s6) <- lex r5,
                                        (x6, r6) <- reads s6,
                                        (",",s7) <- lex r6,
                                        (x7, r7) <- reads s7,
                                        (",",s8) <- lex r7,
                                        (x8, r8) <- reads s8,
                                        (",",s9) <- lex r8,
                                        (x9, r9) <- reads s9,
                                        (",",s10) <- lex r9,
                                        (x10, r10) <- reads s10,
                                        (",",s11) <- lex r10,
                                        (x11, r11) <- reads s11,
                                        (")",w)  <- lex r11 ] )

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g,
          Read h, Read i, Read j, Read k, Read l) =>
         Read (a,b,c,d,e,f,g,h,i,j,k,l)  where
    readsPrec p = readParen False
                    (\r0 -> [((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12), w) |
                                        ("(",s1) <- lex r0,
                                        (x1, r1) <- reads s1,
                                        (",",s2) <- lex r1,
                                        (x2, r2) <- reads s2,
                                        (",",s3) <- lex r2,
                                        (x3, r3) <- reads s3,
                                        (",",s4) <- lex r3,
                                        (x4, r4) <- reads s4,
                                        (",",s5) <- lex r4,
                                        (x5, r5) <- reads s5,
                                        (",",s6) <- lex r5,
                                        (x6, r6) <- reads s6,
                                        (",",s7) <- lex r6,
                                        (x7, r7) <- reads s7,
                                        (",",s8) <- lex r7,
                                        (x8, r8) <- reads s8,
                                        (",",s9) <- lex r8,
                                        (x9, r9) <- reads s9,
                                        (",",s10) <- lex r9,
                                        (x10, r10) <- reads s10,
                                        (",",s11) <- lex r10,
                                        (x11, r11) <- reads s11,
                                        (",",s12) <- lex r11,
                                        (x12, r12) <- reads s12,
                                        (")",w)  <- lex r12 ] )

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g,
          Read h, Read i, Read j, Read k, Read l, Read m) =>
         Read (a,b,c,d,e,f,g,h,i,j,k,l,m)  where
    readsPrec p = readParen False
                    (\r0 -> [((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13), w) |
                                        ("(",s1) <- lex r0,
                                        (x1, r1) <- reads s1,
                                        (",",s2) <- lex r1,
                                        (x2, r2) <- reads s2,
                                        (",",s3) <- lex r2,
                                        (x3, r3) <- reads s3,
                                        (",",s4) <- lex r3,
                                        (x4, r4) <- reads s4,
                                        (",",s5) <- lex r4,
                                        (x5, r5) <- reads s5,
                                        (",",s6) <- lex r5,
                                        (x6, r6) <- reads s6,
                                        (",",s7) <- lex r6,
                                        (x7, r7) <- reads s7,
                                        (",",s8) <- lex r7,
                                        (x8, r8) <- reads s8,
                                        (",",s9) <- lex r8,
                                        (x9, r9) <- reads s9,
                                        (",",s10) <- lex r9,
                                        (x10, r10) <- reads s10,
                                        (",",s11) <- lex r10,
                                        (x11, r11) <- reads s11,
                                        (",",s12) <- lex r11,
                                        (x12, r12) <- reads s12,
                                        (",",s13) <- lex r12,
                                        (x13, r13) <- reads s13,
                                        (")",w)  <- lex r13 ] )

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g,
          Read h, Read i, Read j, Read k, Read l, Read m, Read n) =>
         Read (a,b,c,d,e,f,g,h,i,j,k,l,m,n)  where
    readsPrec p = readParen False
                    (\r0 -> [((x1,x2,x3,x4,x5,x6,x7
                              ,x8,x9,x10,x11,x12,x13,x14), w) |
                                        ("(",s1) <- lex r0,
                                        (x1, r1) <- reads s1,
                                        (",",s2) <- lex r1,
                                        (x2, r2) <- reads s2,
                                        (",",s3) <- lex r2,
                                        (x3, r3) <- reads s3,
                                        (",",s4) <- lex r3,
                                        (x4, r4) <- reads s4,
                                        (",",s5) <- lex r4,
                                        (x5, r5) <- reads s5,
                                        (",",s6) <- lex r5,
                                        (x6, r6) <- reads s6,
                                        (",",s7) <- lex r6,
                                        (x7, r7) <- reads s7,
                                        (",",s8) <- lex r7,
                                        (x8, r8) <- reads s8,
                                        (",",s9) <- lex r8,
                                        (x9, r9) <- reads s9,
                                        (",",s10) <- lex r9,
                                        (x10, r10) <- reads s10,
                                        (",",s11) <- lex r10,
                                        (x11, r11) <- reads s11,
                                        (",",s12) <- lex r11,
                                        (x12, r12) <- reads s12,
                                        (",",s13) <- lex r12,
                                        (x13, r13) <- reads s13,
                                        (",",s14) <- lex r13,
                                        (x14, r14) <- reads s14,
                                        (")",w)  <- lex r14 ] )

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g,
          Read h, Read i, Read j, Read k, Read l, Read m, Read n, Read o) =>
         Read (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)  where
    readsPrec p = readParen False
                    (\r0 -> [((x1,x2,x3,x4,x5,x6,x7
                              ,x8,x9,x10,x11,x12,x13,x14,x15), w) |
                                        ("(",s1) <- lex r0,
                                        (x1, r1) <- reads s1,
                                        (",",s2) <- lex r1,
                                        (x2, r2) <- reads s2,
                                        (",",s3) <- lex r2,
                                        (x3, r3) <- reads s3,
                                        (",",s4) <- lex r3,
                                        (x4, r4) <- reads s4,
                                        (",",s5) <- lex r4,
                                        (x5, r5) <- reads s5,
                                        (",",s6) <- lex r5,
                                        (x6, r6) <- reads s6,
                                        (",",s7) <- lex r6,
                                        (x7, r7) <- reads s7,
                                        (",",s8) <- lex r7,
                                        (x8, r8) <- reads s8,
                                        (",",s9) <- lex r8,
                                        (x9, r9) <- reads s9,
                                        (",",s10) <- lex r9,
                                        (x10, r10) <- reads s10,
                                        (",",s11) <- lex r10,
                                        (x11, r11) <- reads s11,
                                        (",",s12) <- lex r11,
                                        (x12, r12) <- reads s12,
                                        (",",s13) <- lex r12,
                                        (x13, r13) <- reads s13,
                                        (",",s14) <- lex r13,
                                        (x14, r14) <- reads s14,
                                        (",",s15) <- lex r14,
                                        (x15, r15) <- reads s15,
                                        (")",w)  <- lex r15 ] )



