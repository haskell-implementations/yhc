module Data.List
  (delete ,deleteBy ,deleteFirsts ,deleteFirstsBy ,(\\)
  ,elemBy ,elemIndex ,elemIndexBy ,elemIndices
  ,find ,findIndices ,findIndex
  ,genericDrop ,genericLength ,genericReplicate
  ,genericSplitAt ,genericTake ,genericIndex
  ,group ,groupBy
  ,inits ,intersect ,intersectBy ,intersperse ,lookupBy
  ,insert ,insertBy
  ,isPrefixOf ,isSuffixOf
  ,mapAccumL ,mapAccumR ,maximumBy ,minimumBy
  ,notElemBy ,nub ,nubBy
  ,partition ,permutations
  ,sort ,sortBy
  ,subsequences ,tails ,transpose 
  ,sums ,products
  ,unfoldr ,union, unionBy
  ,unzip4 ,unzip5 ,unzip6 ,unzip7
  ,zip4 ,zip5 ,zip6 ,zip7
  ,zipWith4 ,zipWith5 ,zipWith6 ,zipWith7

  -- ... and what the Prelude exports
  ,[] ((:), [])
  ,map, (++), concat, filter
  ,head, last, tail, init, null, length, (!!)
  ,foldl, foldl1, scanl, scanl1, foldr, foldr1, scanr, scanr1
  ,iterate, repeat, replicate, cycle
  ,take, drop, splitAt, takeWhile, dropWhile, span, break
  ,lines, words, unlines, unwords, reverse, and, or
  ,any, all, elem, notElem, lookup
  ,sum, product, maximum, minimum, concatMap
  ,zip, zip3, zipWith, zipWith3, unzip, unzip3
  ) where

import Data.Maybe(listToMaybe)
  
  
-- Originally From Delete.hs  


-- delete x removes the first occurrence of x from its list argument.
delete                  :: (Eq a) => a -> [a] -> [a]
delete                  =  deleteBy (==)
  
  
-- Originally From DeleteBy.hs  

deleteBy                :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy eq x []        = []
deleteBy eq x (y:ys)    = if x `eq` y then ys else y: deleteBy eq x ys
  
  
-- Originally From DeleteFirsts.hs  


-- Alternate name for \\
deleteFirsts            :: (Eq a) => [a] -> [a] -> [a]
deleteFirsts            = (\\)
  
  
-- Originally From DeleteFirstsBy.hs  


deleteFirstsBy          :: (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteFirstsBy eq       =  foldl (flip (deleteBy eq))
  
  
-- Originally From Difference.hs  


infix 5 \\  -- comment is to protect trailing backslash from gcc >= 3.3

-- list difference (non-associative).  In the result of xs \\ ys,
-- the first occurrence of each element of ys in turn (if any)
-- has been removed from xs.  This (xs ++ ys) \\ xs == ys.
(\\)                    :: (Eq a) => [a] -> [a] -> [a]
(\\)                    =  foldl (flip delete)
  
  
-- Originally From ElemBy.hs  

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy eq _ []          =  False
elemBy eq x (y:ys)      =  x `eq` y || elemBy eq x ys
  
  
-- Originally From ElemIndex.hs  


elemIndex               :: Eq a => a -> [a] -> Maybe Int
elemIndex x             =  findIndex (x ==)
  
  
-- Originally From ElemIndexBy.hs  

elemIndexBy             :: (a -> a -> Bool) -> [a] -> a -> Int
elemIndexBy eq [] x      = error "List.elemIndexBy: empty list"
elemIndexBy eq (x:xs) x' = if x `eq` x' then 0 else 1 + elemIndexBy eq xs x'

  
  
-- Originally From ElemIndices.hs  


elemIndices             :: Eq a => a -> [a] -> [Int]
elemIndices x           =  findIndices (x ==)
  
  
-- Originally From Find.hs  


find                    :: (a -> Bool) -> [a] -> Maybe a
find p                  =  listToMaybe . filter p
  
  
-- Originally From FindIndex.hs  


findIndex               :: (a -> Bool) -> [a] -> Maybe Int
findIndex p             =  listToMaybe . findIndices p
  
  
-- Originally From FindIndices.hs  

findIndices             :: (a -> Bool) -> [a] -> [Int]
findIndices p xs        =  [ i | (x,i) <- zip xs [0..], p x ]

  
  
-- Originally From GenericDrop.hs  

genericDrop             :: (Integral i) => i -> [a] -> [a]
genericDrop 0 xs        =  xs
genericDrop _ []        =  []
genericDrop n (_:xs) | n > 0  =  genericDrop (n-1) xs
genericDrop _ _         =  error "List.genericDrop: negative argument"
  
  
-- Originally From GenericIndex.hs  

genericIndex             :: (Integral i) => [a] -> i -> a
genericIndex (x:_) 0      = x
genericIndex (_:xs) n
             | n > 0      = genericIndex xs (n-1)
             | otherwise  = error "List.genericIndex: negative argument"
genericIndex _ _          = error "List.genericIndex: index too large"

  
  
-- Originally From GenericLength.hs  

genericLength           :: (Integral i) => [b] -> i
genericLength []        =  0
genericLength (_:l)     =  1 + genericLength l


  
  
-- Originally From GenericReplicate.hs  


genericReplicate        :: (Integral i) => i -> a -> [a]
genericReplicate n x    =  genericTake n (repeat x)
  
  
-- Originally From GenericSplitAt.hs  

genericSplitAt          :: (Integral i) => i -> [b] -> ([b],[b])
genericSplitAt 0 xs     =  ([],xs)
genericSplitAt _ []     =  ([],[])
genericSplitAt n (x:xs) | n > 0  =  (x:xs',xs'') where
                               (xs',xs'') = genericSplitAt (n-1) xs
genericSplitAt _ _      =  error "List.genericSplitAt: negative argument"
  
  
-- Originally From GenericTake.hs  

genericTake             :: (Integral i) => i -> [a] -> [a]
genericTake 0 _         =  []
genericTake _ []        =  []
genericTake n (x:xs) | n > 0  =  x : genericTake (n-1) xs
genericTake _  _        =  error "List.genericTake: negative argument"

  
  
-- Originally From Group.hs  


-- group splits its list argument into a list of lists of equal, adjacent
-- elements.  e.g.,
-- group "Mississippi" == ["M","i","ss","i","ss","i","pp","i"]
group                   :: (Eq a) => [a] -> [[a]]
group                   =  groupBy (==)
  
  
-- Originally From GroupBy.hs  

groupBy                 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy eq []           =  []
groupBy eq (x:xs)       =  (x:ys) : groupBy eq zs
                           where (ys,zs) = span (eq x) xs
  
  
-- Originally From Inits.hs  

-- inits xs returns the list of initial segments of xs, shortest first.
-- e.g., inits "abc" == ["","a","ab","abc"]
inits                   :: [a] -> [[a]]
inits []                =  [[]]
inits (x:xs)            =  [[]] ++ map (x:) (inits xs)
  
  
-- Originally From Insert.hs  


insert      :: (Ord a) => a -> [a] -> [a]
insert       = insertBy compare

  
  
-- Originally From InsertBy.hs  

insertBy                :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy cmp x []       =  [x]
insertBy cmp x ys@(y:ys') 
                        =  case cmp x y of
                           GT -> y : insertBy cmp x ys'
                           _  -> x : ys
  
  
-- Originally From Intersect.hs  


intersect    :: (Eq a) => [a] -> [a] -> [a]
intersect     =  intersectBy (==)


  
  
-- Originally From IntersectBy.hs  

intersectBy            :: (a->a->Bool) -> [a] -> [a] -> [a]
intersectBy eq xs ys    =  [x | x <- xs, any (eq x) ys]
  
  
-- Originally From Intersperse.hs  

-- intersperse sep inserts sep between the elements of its list argument.
-- e.g. intersperse ',' "abcde" == "a,b,c,d,e"
intersperse             :: a -> [a] -> [a]
intersperse sep []      =  []
intersperse sep [x]     =  [x]
intersperse sep (x:xs)  =  x : sep : intersperse sep xs
  
  
-- Originally From IsPrefixOf.hs  

isPrefixOf               :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _           =  True
isPrefixOf (_:_)   []     =  False
isPrefixOf (x:xs) (y:ys)  =  x == y && isPrefixOf xs ys

  
  
-- Originally From IsSuffixOf.hs  


isSuffixOf        :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf x y     =  reverse x `isPrefixOf` reverse y

  
  
-- Originally From LookupBy.hs  

lookupBy                :: (a -> a -> Bool) -> a -> [(a, b)] -> Maybe b
lookupBy eq key []      =  Nothing
lookupBy eq key ((x,y):xys)
    | key `eq` x        =  Just y
    | otherwise         =  lookupBy eq key xys
  
  
-- Originally From MapAccumL.hs  

mapAccumL               :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccumL f s []        =  (s, [])
mapAccumL f s (x:xs)    =  (s'',y:ys)
                           where (s', y ) = f s x
                                 (s'',ys) = mapAccumL f s' xs
  
  
-- Originally From MapAccumR.hs  

mapAccumR               :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccumR f s []        =  (s, [])
mapAccumR f s (x:xs)    =  (s2, z:zs)
                           where (s2, z ) = f s1 x
                                 (s1, zs) = mapAccumR f s xs

  
  
-- Originally From MaximumBy.hs  

maximumBy               :: (a -> a -> Ordering) -> [a] -> a
maximumBy cmp []        =  error "List.maximumBy: empty list"
maximumBy cmp xs        =  foldl1 max xs
                 where max x y = case cmp x y of
                                                  GT -> x
                                                  _  -> y
  
  
-- Originally From MinimumBy.hs  

minimumBy               :: (a -> a -> Ordering) -> [a] -> a
minimumBy cmp []        =  error "List.minimumBy: empty list"
minimumBy cmp xs        =  foldl1 min xs
                           where min x y = case cmp x y of
                                                  GT -> y
                                                  _  -> x
  
  
-- Originally From NotElemBy.hs  


notElemBy       :: (a -> a -> Bool) -> a -> [a] -> Bool
notElemBy eq x xs       =  not (elemBy eq x xs)
  
  
-- Originally From Nub.hs  


-- nub (meaning "essence") remove duplicate elements from its list argument.
nub                     :: (Eq a) => [a] -> [a]
nub                     =  nubBy (==)
  
  
-- Originally From NubBy.hs  

nubBy                   :: (a -> a -> Bool) -> [a] -> [a]
nubBy eq []             =  []
nubBy eq (x:xs)         =  x : nubBy eq (filter (\ y -> not (eq x y)) xs)
  
  
-- Originally From Partition.hs  

-- partition takes a predicate and a list and returns a pair of lists:
-- those elements of the argument list that do and do not satisfy the
-- predicate, respectively; i,e,,
--
-- partition p xs == (filter p xs, filter (not . p) xs).

partition               :: (a -> Bool) -> [a] -> ([a],[a])
partition p xs          =  foldr select ([],[]) xs
                           where select x ~(ts,fs) | p x       = (x:ts,fs)
                                                   | otherwise = (ts, x:fs)
  
  
-- Originally From Permutations.hs  

-- permutations xs returns the list of all permutations of xs.
-- e.g., permutations "abc" == ["abc","bac","bca","acb","cab","cba"]
permutations            :: [a] -> [[a]]
permutations []         =  [[]]
permutations (x:xs)     =  [zs | ys <- permutations xs, zs <- interleave x ys ]
  where interleave          :: a -> [a] -> [[a]]
        interleave x []     =  [[x]]
        interleave x (y:ys) =  [x:y:ys] ++ map (y:) (interleave x ys)
  
  
-- Originally From Products.hs  

products          :: (Num a) => [a] -> [a]
products                =  scanl (*) 1 
  
  
-- Originally From Sort.hs  


-- stable sorting algorithm

sort                    :: (Ord a) => [a] -> [a]
sort                    =  sortBy compare
  
  
-- Originally From SortBy.hs  

-- stable sorting algorithm

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = mergeAll . sequences
  where
    sequences (a:b:xs)
      | a `cmp` b == GT = descending b [a]  xs
      | otherwise       = ascending  b [a] xs
    sequences xs = [xs]

    descending a as (b:bs)
      | a `cmp` b == GT = descending b (a:as) bs
    descending a as bs  = (a:as): sequences bs

    ascending a as (b:bs)
      | a `cmp` b /= GT = ascending b (a:as) bs
    ascending a as bs   = rev as [a] : sequences bs

    rev (x:xs) ys = rev xs (x:ys)
    rev [] ys = ys

    mergeAll [x] = x
    mergeAll xs  = mergeAll (mergePairs xs)

    mergePairs (a:b:xs) = merge a b: mergePairs xs
    mergePairs xs       = xs

    merge as@(a:as') bs@(b:bs')
      | a `cmp` b == GT = b:merge as  bs'
      | otherwise       = a:merge as' bs
    merge [] bs         = bs
    merge as []         = as
  
  
-- Originally From Subsequences.hs  

-- subsequences xs returns the list of all subsequences of xs.
-- e.g., subsequences "abc" == ["","c","b","bc","a","ac","ab","abc"]
subsequences            :: [a] -> [[a]]
subsequences []         =  [[]]
subsequences (x:xs)     =  subsequences xs ++ map (x:) (subsequences xs)
  
  
-- Originally From Sums.hs  

sums          :: (Num a) => [a] -> [a]
sums                    =  scanl (+) 0 
  
  
-- Originally From Tails.hs  

-- tails xs returns the list of all final segments of xs, longest first.
-- e.g., tails "abc" == ["abc", "bc", "c",""]
tails                   :: [a] -> [[a]]
tails []                =  [[]]
tails xxs@(_:xs)        =  xxs : tails xs
  
  
-- Originally From Transpose.hs  

-- transpose is lazy in both rows and columns, and works for
--    non-rectangular 'matrices'
-- Note that [h | (h:t) <- xss] is not the same as (map head xss)
--    because the former discards empty sublists inside xss

transpose       :: [[a]] -> [[a]]
transpose []            =  []
transpose ([]:    xss)  =  transpose xss
transpose ((x:xs):xss)  =  (x: [h | (h:t)<-xss]) :
                           transpose (xs: [t | (h:t) <- xss])
  
  
-- Originally From Unfoldr.hs  

unfoldr      :: (b -> Maybe (a,b)) -> b -> [a]
unfoldr f b   = case (f b) of
                    Nothing      -> []
                    (Just (a,b)) -> a : unfoldr f b

  
  
-- Originally From Union.hs  


union       :: (Eq a) => [a] -> [a] -> [a]
union        =  unionBy (==)

  
  
-- Originally From UnionBy.hs  


unionBy           :: (a->a->Bool) -> [a] -> [a] -> [a]
unionBy eq xs ys   =  xs ++  deleteFirstsBy eq (nubBy eq ys) xs

  
  
-- Originally From Unzip4.hs  

unzip4  :: [(a,b,c,d)] -> ([a],[b],[c],[d])
unzip4  = foldr (\(b,c,d,e) ~(bs,cs,ds,es) -> (b:bs,c:cs,d:ds,e:es))
                             ([],[],[],[])
  
  
-- Originally From Unzip5.hs  

unzip5 :: [(a,b,c,d,e)] -> ([a],[b],[c],[d],[e])
unzip5 = foldr (\(b,c,d,e,f) ~(bs,cs,ds,es,fs) -> (b:bs,c:cs,d:ds,e:es,f:fs))
                              ([],[],[],[],[])

  
  
-- Originally From Unzip6.hs  

unzip6 :: [(a,b,c,d,e,f)] -> ([a],[b],[c],[d],[e],[f])
unzip6 = foldr (\(b,c,d,e,f,g) ~(bs,cs,ds,es,fs,gs) ->
                                (b:bs,c:cs,d:ds,e:es,f:fs,g:gs))
                                ([],[],[],[],[],[])

  
  
-- Originally From Unzip7.hs  

unzip7 :: [(a,b,c,d,e,f,g)] -> ([a],[b],[c],[d],[e],[f],[g])
unzip7 = foldr (\(b,c,d,e,f,g,h) ~(bs,cs,ds,es,fs,gs,hs) ->
                                  (b:bs,c:cs,d:ds,e:es,f:fs,g:gs,h:hs))
                                  ([],[],[],[],[],[],[])

  
  
-- Originally From Zip4.hs  


zip4    :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)] 
zip4    = zipWith4(,,,)
  
  
-- Originally From Zip5.hs  


zip5    :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)] 
zip5    = zipWith5 (,,,,)
  
  
-- Originally From Zip6.hs  


zip6    :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a,b,c,d,e,f)] 
zip6    = zipWith6 (,,,,,)
  
  
-- Originally From Zip7.hs  


zip7    :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [(a,b,c,d,e,f,g)] 
zip7    = zipWith7 (,,,,,,)
  
  
-- Originally From ZipWith4.hs  

zipWith4                 :: (a->b->c->d->e) -> [a]->[b]->[c]->[d]->[e]
zipWith4 z (b:bs) (c:cs) (d:ds) (e:es)
                          = z b c d e : zipWith4 z bs cs ds es
zipWith4 _ _ _ _ _        = []

  
  
-- Originally From ZipWith5.hs  

zipWith5                 :: (a->b->c->d->e->f) -> [a]->[b]->[c]->[d]->[e]->[f]
zipWith5 z (b:bs) (c:cs) (d:ds) (e:es) (f:fs)
                          = z b c d e f : zipWith5 z bs cs ds es fs
zipWith5 _ _ _ _ _ _      = []

  
  
-- Originally From ZipWith6.hs  

zipWith6                 :: (a->b->c->d->e->f->g)
                            -> [a]->[b]->[c]->[d]->[e]->[f]->[g]
zipWith6 z (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) 
                          = z b c d e f g : zipWith6 z bs cs ds es fs gs
zipWith6 _ _ _ _ _ _ _    = []

  
  
-- Originally From ZipWith7.hs  

zipWith7                 :: (a->b->c->d->e->f->g->h)
                             -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]
zipWith7 z (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs)
                          = z b c d e f g h : zipWith7 z bs cs ds es fs gs hs
zipWith7 _ _ _ _ _ _ _ _  = []

