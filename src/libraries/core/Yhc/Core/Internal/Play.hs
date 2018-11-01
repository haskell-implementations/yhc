
module Yhc.Core.Internal.Play where

import Control.Monad

class Play a where
    getChildren :: a -> [a]
    setChildren :: a -> [a] -> a


allChildren :: Play a => a -> [a]
allChildren x = x : concatMap allChildren (getChildren x)

-- bottom up mapping
mapUnder :: Play a => (a -> a) -> a -> a
mapUnder f x = f $ setChildren x $ map (mapUnder f) $ getChildren x

mapUnderM :: (Monad m, Play a) => (a -> m a) -> a -> m a
mapUnderM f x = f =<< (liftM (setChildren x) $ mapM (mapUnderM f) $ getChildren x)

-- top down mapping
mapOver :: Play a => (a -> a) -> a -> a
mapOver f x = setChildren x2 $ map (mapOver f) $ getChildren x2
    where x2 = f x
