module Control.Monad
  ( MonadPlus(mzero,mplus)
  , join, guard, unless, when, ap
  , msum
  , filterM, mapAndUnzipM, zipWithM, zipWithM_, foldM
  , liftM, liftM2, liftM3, liftM4, liftM5

  -- ... and what the Prelude exports
  , Monad((>>=), (>>), return, fail)
  , Functor(fmap)
  , mapM, mapM_, sequence, sequence_, (=<<)
  , replicateM, replicateM_
  , forever
  ) where


  
  
-- Originally From Ap.hs  


ap   :: Monad m => m (a->b) -> m a -> m b
ap    = liftM2 ($)
  
  
-- Originally From CMonadPlus.hs  

class (Monad m) => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a
  
  
-- Originally From FilterM.hs  

filterM          :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM p []      = return []
filterM p (x:xs)  = do b <- p x
                       ys <- filterM p xs
                       return (if b then (x:ys) else ys)
  
  
-- Originally From FoldM.hs  

foldM            :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM f a []     =  return a
foldM f a (x:xs) =  f a x >>= \y-> foldM f y xs

  
  
-- Originally From Guard.hs  


guard       :: MonadPlus m => Bool -> m ()
guard p     = if p then return () else mzero
  
  
-- Originally From Join.hs  

join             :: (Monad m) => m (m a) -> m a
join x           = x >>= id
  
  
-- Originally From LiftM.hs  

liftM      :: Monad m => (a->b) -> (m a -> m b)
liftM f     = \a-> do a' <- a
                      return (f a')
  
  
-- Originally From LiftM2.hs  

liftM2      :: Monad m => (a->b->c) -> (m a -> m b -> m c)
liftM2 f     = \a b -> do a' <- a
                          b' <- b
                          return (f a' b')
  
  
-- Originally From LiftM3.hs  

liftM3      :: Monad m => (a->b->c->d) -> (m a -> m b -> m c -> m d)
liftM3 f     = \a b c -> do a' <- a
                            b' <- b
                            c' <- c
                            return (f a' b' c')
  
  
-- Originally From LiftM4.hs  

liftM4      :: Monad m => (a->b->c->d->e) -> (m a -> m b -> m c -> m d -> m e)
liftM4 f     = \a b c d -> do a' <- a
                              b' <- b
                              c' <- c
                              d' <- d
                              return (f a' b' c' d')
  
  
-- Originally From LiftM5.hs  

liftM5      :: Monad m => (a->b->c->d->e->f) ->
                          (m a -> m b -> m c -> m d -> m e -> m f)
liftM5 f     = \a b c d e -> do a' <- a
                                b' <- b
                                c' <- c
                                d' <- d
                                e' <- e
                                return (f a' b' c' d' e')
  
  
-- Originally From MapAndUnzipM.hs  

mapAndUnzipM     :: (Monad m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
mapAndUnzipM f xs = sequence (map f xs) >>= return . unzip
  
  
-- Originally From MonadPlus_List.hs  


instance  MonadPlus []  where
    mzero  =  []
    mplus  =  (++)
  
  
-- Originally From MonadPlus_Maybe.hs  


instance  MonadPlus Maybe  where
    mzero  =  Nothing

    Nothing `mplus` ys       = ys
    xs      `mplus` ys       = xs



  
  
-- Originally From Msum.hs  


msum      :: MonadPlus m => [m a] -> m a
msum xs    =  foldr mplus mzero xs

  
  
-- Originally From Unless.hs  


unless           :: (Monad m) => Bool -> m () -> m ()
unless p s       =  when (not p) s
  
  
-- Originally From When.hs  

when             :: (Monad m) => Bool -> m () -> m ()
when p s         =  if p then s else return ()
  
  
-- Originally From ZipWithM.hs  

zipWithM         :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys =  sequence (zipWith f xs ys)
  
  
-- Originally From ZipWithM_.hs  

zipWithM_         :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ f xs ys =  sequence_ (zipWith f xs ys)

-- Stolen from GHC

-- | @'replicateM' n act@ performs the action @n@ times,
-- gathering the results.
replicateM        :: (Monad m) => Int -> m a -> m [a]
replicateM n x    = sequence (replicate n x)

-- | Like 'replicateM', but discards the result.
replicateM_       :: (Monad m) => Int -> m a -> m ()
replicateM_ n x   = sequence_ (replicate n x)

-- | @'forever' act@ repeats the action infinitely.
forever     :: (Monad m) => m a -> m ()
forever a   = a >> forever a

