
module Yhc.Core.Play where

import Yhc.Core.Type
import Yhc.Core.Internal.Play

import Control.Exception
import Control.Monad


-- | All the variables in a CoreExpr
allCoreVar :: CoreExpr -> [String]
allCoreVar x = [i | CoreVar i <- allCore x]


class PlayCore a where
    getChildrenCore :: a -> [CoreExpr]
    setChildrenCore :: a -> [CoreExpr] -> a


    allCore :: a -> [CoreExpr]
    allCore x = concatMap allCore (getChildrenCore x)

    mapOverCore :: (CoreExpr -> CoreExpr) -> a -> a
    mapOverCore f x = setChildrenCore x $ map (mapOverCore f) $ getChildrenCore x

    mapUnderCore :: (CoreExpr -> CoreExpr) -> a -> a
    mapUnderCore f x = setChildrenCore x $ map (mapUnderCore f) $ getChildrenCore x

    mapUnderCoreM :: Monad m => (CoreExpr -> m CoreExpr) -> a -> m a
    mapUnderCoreM f x = liftM (setChildrenCore x) $ mapM (mapUnderCoreM f) $ getChildrenCore x


instance Play CoreExpr where
    getChildren = getChildrenCore
    setChildren = setChildrenCore


instance PlayCore CoreExpr where
    getChildrenCore x =
        case x of
            CoreApp x xs -> x:xs
            CoreCase x xs -> (x : map snd xs) 
            CoreLet x xs -> xs: map snd x
            CoreLam x xs -> [xs]
            CorePos x xs -> [xs]
            _ -> []

    setChildrenCore x ys =
        case x of
            CoreApp _ _ -> CoreApp (head ys) (tail ys)

            CoreCase _ xs -> CoreCase (head ys) (zip (map fst xs) (tail ys))

            CoreLet x _ -> CoreLet (zip (map fst x) (tail ys)) (head ys)

            CoreLam x _ -> let [y1] = ys in CoreLam x y1

            CorePos p _ -> let [y1] = ys in CorePos p y1

            x -> assert (null ys) x


    allCore = allChildren
    mapOverCore = mapOver
    mapUnderCore = mapUnder
    mapUnderCoreM = mapUnderM


instance PlayCore a => PlayCore [a] where
    getChildrenCore x = concatMap getChildrenCore x

    setChildrenCore [] [] = []
    setChildrenCore (x:xs) ys = setChildrenCore x a : setChildrenCore xs b
        where (a,b) = splitAt (length $ getChildrenCore x) ys


instance PlayCore CoreFunc where
    getChildrenCore (CoreFunc a b c) = [c]
    getChildrenCore x = []

    setChildrenCore (CoreFunc a b _) [c] = CoreFunc a b c
    setChildrenCore x [] = x


instance PlayCore CoreData where
    getChildrenCore _ = []
    setChildrenCore x [] = x


instance PlayCore Core where
    getChildrenCore (Core a b c d) = getChildrenCore d
    setChildrenCore (Core a b c d) ys = Core a b c $ setChildrenCore d ys
