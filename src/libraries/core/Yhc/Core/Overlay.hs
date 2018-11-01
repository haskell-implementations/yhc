
module Yhc.Core.Overlay(coreOverlay) where

import Yhc.Core.Type
import Yhc.Core.Uniplate
import Yhc.Core.Prim

import qualified Data.Set as Set
import Data.List
import Data.Char


-- | coreOverlay original overlay, returns original with the overlay substituted in
coreOverlay :: Core -> Core -> Core
coreOverlay original overlay = original
        {coreDatas = filter localData (coreDatas overlay2) ++ coreDatas original
        ,coreFuncs = coreFuncs overlay2 ++ filter (not . (`Set.member` ignore) . coreFuncName) (coreFuncs original)}
    where
        overlay2 = decodeOverlay overlay
        ignore = Set.fromList $ map coreFuncName $ coreFuncs overlay2
        localData = not . isPrefixOf "Global_" . dropModNames . coreDataName


decodeOverlay :: Core -> Core
decodeOverlay core = core{coreFuncs = transformExpr f $ map g $ coreFuncs core}
    where
        g func = func{coreFuncName = decodeString $ coreFuncName func}
        
        f (CoreFun x) = CoreFun $ decodeString x
        f (CoreCon x) = CoreCon $ decodeString x
        f x = x



names = [";'","'ap","._","=eq",">gt","<lt","&amp","|pip","^hat","!ex",":col","%per"]


decodeString :: String -> String
decodeString x | "global_" `isPrefixOf` map toLower x2 = f (drop 7 x2)
               | otherwise = x
    where
        x2 = dropModNames x
    
        f ('\'':xs) | not (null chrs) = let (y,ys) = head chrs in y : f (drop (length ys) xs)
            where chrs = [(y,ys) | y:ys <- names, ys `isPrefixOf` xs]
        f (x:xs) = x : f xs
        f [] = []


dropModNames :: String -> String
dropModNames = reverse . takeWhile (/= ';') . reverse

