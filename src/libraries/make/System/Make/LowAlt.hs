{-|
    Low level interface to a Make like system. This module is intended
    for programs to use, but not for people to write rules in.

    Older version with support for alternative file locations,
    which is probably not needed ever.
-}
module System.Make.LowAlt where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Ord
import Data.IORef
import System.Directory
import System.Time
import Control.Monad


type Key = String


data File = File {fileKey :: Key, file :: FilePath}


type Action = (Key -> [FilePath]) -> IO ()


data Rule = Rule {action   :: Action
                 ,requires :: [File]
                 ,produces :: [File]
                 }
          | Alts {name :: FilePath
                 ,alts :: [FilePath]
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
    (bind1,older) <- followAlts state requires
    (bind2,create) <- followAlts state produces
    let binds = bind1++bind2
    -- can assume that all exist ones really do exist
    -- must check each older one is old enough
    skip <- do
        b <- andM $ map (testFileExists state) create
        if not b then return False else do
            minCreate <- liftM minimum $ mapM (getFileTime state) create
            andM [liftM (< minCreate) $ getFileTime state a | a <- older]

    when (not skip) $ do
        now <- getClockTime
        setFileTimes state (map (flip (,) now) create)
        let ask k = [b | (a,b) <- binds, a == k]
        action ask


followAlts :: StateIO -> [File] -> IO ([(Key,FilePath)], [FilePath])
followAlts state xs = mapAndUnzipM f xs
    where
        f (File key val) = do x <- g val ; return ((key,x),x)

        g val = do
            alts <- getAlt state val
            case alts of
                    [x] -> return x
                    [] -> error $ "Can't find an alt for: " ++ val
                    xs@(x:_) -> h x xs

        h x [] = return x
        h x (y:ys) = do
            b <- testFileExists state y
            if b then return y else h x ys


dischargeTargets :: StateIO -> [FilePath] -> IO ()
dischargeTargets state targets = mapM_ f targets
    where
        f x = g x =<< getAlt state x

        g x [] = error $ "Make failed, could not find: " ++ x
        g x (y:ys) = do
            b <- testFileExists state y
            if b then
                setAlt state x y
             else
                g x ys


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
                                ((targets \\ map file (produces rule)) `union` newTargets)
            where
                matches = [(n,(i,r)) | (i,r) <- irules, not $ i `Set.member` usedRules
                                     , let n = length $ map file (produces r) `intersect` targets, n > 0]
                (irule,rule) = snd $ maximumBy (comparing fst) matches

                newTargets = map file $ requires rule
                overlap = Set.intersection usedTargets (Set.fromList newTargets)


---------------------------------------------------------------------
-- State Operations

type StateIO = IORef State

data State = State (Map.Map FilePath [FilePath]) -- alts mappings
                   (Map.Map FilePath FileStat) -- stat cache

data FileStat = ExistYes
              | ExistNo
              | TimeStamp ClockTime


initState :: [Rule] -> IO StateIO
initState rules = newIORef $ State (Map.fromList [(a,b) | Alts a b <- rules]) Map.empty


setAlt :: StateIO -> FilePath -> FilePath -> IO ()
setAlt state x y = modifyIORef state f
    where f (State a b) = State (Map.insert x [y] a) b


getAlt :: StateIO -> FilePath -> IO [FilePath]
getAlt state x = do
    State a b <- readIORef state
    return $ Map.findWithDefault [x] x a


testFileExists :: StateIO -> FilePath -> IO Bool
testFileExists state x = do
    State a b <- readIORef state
    case Map.lookup x b of
        Just ExistYes -> return True
        Just ExistNo -> return False
        Just (TimeStamp _) -> return True
        Nothing -> do
            e <- doesFileExist x
            writeIORef state $ State a (Map.insert x (if e then ExistYes else ExistNo) b)
            return e


getFileTime :: StateIO -> FilePath -> IO ClockTime
getFileTime state x = do
    State a b <- readIORef state
    case Map.lookup x b of
        Just (TimeStamp x) -> return x
        Just ExistYes -> do
            t <- getModificationTime x
            writeIORef state $ State a (Map.insert x (TimeStamp t) b)
            return t
        _ -> error $ "Invariant violated, getFileTime: " ++ x


setFileTimes :: StateIO -> [(FilePath,ClockTime)] -> IO ()
setFileTimes state xs = modifyIORef state (f xs)
    where
        f [] x = x
        f ((x,y):xs) (State a b) = f xs (State a (Map.insert x (TimeStamp y) b))


---------------------------------------------------------------------
-- General Functions

andM :: Monad m => [m Bool] -> m Bool
andM [] = return True
andM (x:xs) = do
    b <- x
    if b then andM xs else return False
