{-|
    Low level interface to a Make like system. This module is intended
    for programs to use, but not for people to write rules in.

    Older version with support for alternative file locations,
    which is probably not needed ever.
-}
module System.Make.Low where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Ord
import Data.IORef
import System.Directory
import System.Time
import Control.Monad


data Rule = Rule {action   :: IO ()
                 ,requires :: [FilePath]
                 ,produces :: [FilePath]
                 }


-- 1) pick the rule which provides most targets
--    * a rule may not be fired more than once
--    * if a target reoccurs, you have a cycle
--
-- 2) after picking, all left over targets MUST exist
--    * those which exist as alts are frozen in
--
-- 3) run through rules from the bottom, running all
--    which have outstanding dependencies
--
-- NEVER fire a rule more than once
-- NEVER lookup a file more than once (use the cache)
--
runMake :: [FilePath] -> [Rule] -> IO ()
runMake targets rules = do
    state <- initState rules
    (targets,rules) <- return $ orderRules targets rules
    dischargeTargets state targets
    mapM_ (executeRule state) rules


executeRule :: StateIO -> Rule -> IO ()
executeRule state (Rule action requires produces) = do
    -- can assume that all exist ones really do exist
    -- must check each older one is old enough
    skip <- do
        b <- andM $ map (testFileExists state) produces
        if not b then return False else do
            minCreate <- liftM minimum $ mapM (getFileTime state) produces
            andM [liftM (< minCreate) $ getFileTime state a | a <- requires]

    when (not skip) $ do
        now <- getClockTime
        setFileTimes state (map (flip (,) now) produces)
        action


dischargeTargets :: StateIO -> [FilePath] -> IO ()
dischargeTargets state targets = mapM_ f targets
    where
        f x = do
            b <- testFileExists state x
            when (not b) $ error $ "Make failed, could not find: " ++ x


-- given a set of targets, and a set of rules
-- figure out which targets are left over
-- and which rules should be run in what order
--
-- no rule should occur more than once
orderRules :: [FilePath] -> [Rule] -> ([FilePath],[Rule])
orderRules targets rules = f (Set.fromList targets) Set.empty targets
    where
        irules = zip [0..] rules

        f usedTargets usedRules targets
                | null matches = (targets,reverse rules)
                | not $ Set.null overlap =
                    error $ "Make failed, cyclic dependency: " ++ unwords (Set.toList overlap)
                | otherwise = f (Set.union (Set.fromList newTargets) usedTargets)
                                (Set.insert irule usedRules)
                                ((targets \\ produces rule) `union` newTargets)
            where
                matches = [(n,(i,r)) | (i,r) <- irules, not $ i `Set.member` usedRules
                                     , let n = length $ produces r `intersect` targets, n > 0]
                (irule,rule) = snd $ maximumBy (comparing fst) matches

                newTargets = requires rule
                overlap = Set.intersection usedTargets (Set.fromList newTargets)


---------------------------------------------------------------------
-- State Operations

type StateIO = IORef State

type State = Map.Map FilePath FileStat

data FileStat = ExistYes
              | ExistNo
              | TimeStamp ClockTime


initState :: [Rule] -> IO StateIO
initState rules = newIORef Map.empty


testFileExists :: StateIO -> FilePath -> IO Bool
testFileExists state x = do
    s <- readIORef state
    case Map.lookup x s of
        Just ExistYes -> return True
        Just ExistNo -> return False
        Just (TimeStamp _) -> return True
        Nothing -> do
            e <- doesFileExist x
            writeIORef state $ Map.insert x (if e then ExistYes else ExistNo) s
            return e


getFileTime :: StateIO -> FilePath -> IO ClockTime
getFileTime state x = do
    s <- readIORef state
    case Map.lookup x s of
        Just (TimeStamp x) -> return x
        Just ExistYes -> do
            t <- getModificationTime x
            writeIORef state $ Map.insert x (TimeStamp t) s
            return t
        _ -> error $ "Invariant violated, getFileTime: " ++ x


setFileTimes :: StateIO -> [(FilePath,ClockTime)] -> IO ()
setFileTimes state xs = modifyIORef state (f xs)
    where
        f []         s = s
        f ((x,y):xs) s = f xs (Map.insert x (TimeStamp y) s)


---------------------------------------------------------------------
-- General Functions

andM :: Monad m => [m Bool] -> m Bool
andM [] = return True
andM (x:xs) = do
    b <- x
    if b then andM xs else return False
