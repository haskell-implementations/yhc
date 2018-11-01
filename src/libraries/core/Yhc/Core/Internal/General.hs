
module Yhc.Core.Internal.General where

import qualified Data.Set as Set


ordNub :: Ord a => [a] -> [a]
ordNub xs = f Set.empty xs
    where
        f set (x:xs) | x `Set.member` set = f set xs
                     | otherwise          = x : f (Set.insert x set) xs
        f set [] = []

disjoint :: Eq a => [a] -> [a] -> Bool
disjoint x y = not $ any (`elem` x) y

