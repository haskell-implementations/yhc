-- A standalone ycr/yca converter to Javascript

module Main where

import Control.Monad.Reader
import System.Environment
import System.Directory
import System.FilePath
import System.Exit
import System.IO
import Control.Monad
import Yhc.Core
import Data.Char
import Data.List
import Data.Maybe
import Numeric
import Prim
import qualified Data.Map as M
import JS.CorePrep
import JS.CoreJS
import JS.OptOpt
import JS.Show

stdOverlay = "lib/haskell/StdOverlay.ycr" -- hardcode for now

ssaOverlay = "lib/haskell/SSAOverlay.ycr" -- hardcode for now

-- Stolen from Yhc's Main.hs: use the same base path as Yhc does.

getBasePath :: IO String
getBasePath = catch (getEnv "YHC_BASE_PATH") errHandle
    where
    errHandle e = do
        res <- getProgName >>= findExecutable
        case res of
            Nothing -> do
                putStrLn $ "Warning: the environment variable YHC_BASE_PATH is not set\n" ++
                           "         and yhc cannot be found on the path"
                return ""
            Just x -> return $ takeDirectory $ takeDirectory x

main = do
  args <- getArgs
  base <- getBasePath
  prog <- getProgName
  let jsof = defJSOF
  let ovfile = case oCodeGen jsof of
        StdCodeGen -> stdOverlay
        AltCodeGen -> ssaOverlay
  let ovly = base </> ovfile
  when (length args == 0) $ do 
    hPutStrLn stderr $ "usage: " ++ prog ++ "core [root root ...]"
    exitWith (ExitFailure 1)
  let (incore : roots0) = args ++ (map (\_ -> "") [1 .. ])
      roots = takeWhile (\x -> length x > 0) roots0
  when (length roots == 0) $ do
    hPutStrLn stderr $ "warning: no root names specified"
  core <- loadCore incore
  ovcore <- catch (loadCore ovly) (\e -> do
    hPutStrLn stderr $ "Could not load core from " ++ ovly ++ ": " ++ show e
    return $ Core {coreName = "",
                   coreImports = [],
                   coreDatas = [],
                   coreFuncs = []})
  let cconv = (if oCoreSimplify jsof then coreSimplify else id) .
              (if oCaseElim jsof then coreCaseElim else id) .
              (if oRecursiveLet jsof then removeRecursiveLet else id) .
              coreInline (oCoreInline jsof) .
              coreReachable roots  
  let core'' = cconv $ coreOverlay core ovcore
      core' = if oArityAlias jsof
                then (coreReachable (map coreFuncName $ coreFuncs core'') . arityAlias) core''
                else core''
  putStrLn "\n/******* Generated Code: Do Not Edit *******/"
  let conmap = buildConMap core'
      funmap = buildFunMap core'
      varmap = buildVarMap core'
      elimap = buildEliMap core' jsof
      kf = (M.fromList . 
            (map (\(a, b) -> (b, show a))) . 
            M.assocs) (funmap `M.difference` elimap)
      core'' = (indexFunDefs funmap varmap . 
                mapVarNames varmap .                
                mapFunNames funmap .
                mapConNames conmap .
                fixArityAlias funmap) core'
  writeMapIdx stdout show conmap "conIdx"
  writeMapIdx stdout show kf "strIdx"
  let corejs = core2JS jsof core''
  mapM (putStrLn . strJcode) corejs
  when (oKeepStrIdx jsof) $
    writeMapIdx stdout show (M.filterWithKey (\a b -> a `elem` roots) funmap) "funIdx"



