{-

  Compiler Driver for Yhc Web Service.

  Based on: 

  SearchPath.hs v. 0.91
  Contact: alex@HAppS.org

  Copyright (c) 2008, Dmitry Golubovsky, (c) 2007, HAppS LLC

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  * Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.
  * Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  * Neither the name of HAppS LLC nor the names of its contributors may be used
    to endorse or promote products derived from this software without specific
    prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


  See usage at http://searchpath.org/usage.txt

-}

module Main where

{--
Major Changes:
  * no automatic recompile: Yhc compiles only once
  * command line options syntax adjusted for Yhc
  * removed any references to GHC(i): they are not supported by this version
  * default map is downloaded from the local couchdb storage
  * curl runs with no proxy for local couchdb storage, environment setting is used for
    everything else
  * cache directory is .sp-cache again
  * -- End of Yhc-specific changes
  * handling darcs/svn/subversion repos 
  * handling stuff that is inside tgz files at urls
  * additions to the module map for new stuff that is present in the universe 
  * much cleaner/more-readable/maintainable code -- almost ready to support other lanuages?
  * substanitially faster
  * nicer command line options
  * better usage documentation
  * handle lonlocal haskell files
  * run args option to automatically recompile and rerun on source file changes
  * filter .sp-cache should be filter .dirs in general
  * negative caching -- look for url file in the directory.  if present and young enough skip
  * supports command sequences to build the pulled repo
  * add build cmd to tgz  
  * make .sp-cache default to user's homedir 
  * provide module map to executable so it knows where to get any files it came with
  * FIXED BUG: won't run intpereter is executable exists
  * make searchpath autorecompile on source changes if compile failed too
  * write defaulth.map into current directory and use searchpath.default.map if present
  * FIXED if executable exists and no run?
  * now all users of cachedir share the same view formally. mapfiles are irrelevant
  * maintain invariant that changes only via changes to default.map or source hierarchies

Bugs:
  * added -i is too long for stuff that isn't used e.g. just "import Data.Binary"

--}

import System.Cmd (rawSystem)
import Text.Regex
import System.Time
import System.Directory
import Data.List
import System.Environment
import Network.URI (escapeURIString)
import System.IO
import Control.Monad 
import System.Exit
import System.Process
import Data.Maybe
import qualified Data.Set as Set
import qualified Control.Exception as E
import Control.Concurrent

sys cmd@(a:xs) =
    do
    rawSystem a xs

main = getArgs >>= main'

debug x conf msg f = when (verbose conf > x) $ print msg >> f >> return ()

main' args = do
  if args `elem` [[], ["-h"],["--help"]] then printUsage else do
  conf <- prepConf defaultConf args
  modLocs <- return . concat =<< mapM (getModLocs conf) (mapLocs conf) 
  debug 1 conf "MODLOCS" $ mapM print modLocs
  conf <- return conf {modLocs = modLocs}
  hsFiles' <- (mapM (moduleChase Set.empty conf modLocs) $ hsFiles conf) >>=
              return . map fromJust . filter isJust . map snd
  conf <- return conf {hsFiles=hsFiles'}
  print $ confToCmdLine conf
  let cmdLine = confToCmdLine conf
  sys $ words cmdLine
  return ()

version = "0.91/yhc"
printUsage = do putStr $ "SearchPath/Yhc "++version ++ "\n\n"
                putStr $ "For use within Yhc Web Service: will not work with GHC[i]\n\n" 
                return ()

{--
source files changed in -i dirs not full recompile
--}
hasNewer fp dirs = do
  fe <- doesFileExist fp
  if not fe then return True else do
  TOD ft _ <- getModificationTime fp
  modSince ft dirs
modSince t [] = return False
modSince t (fp:fps)= do
  fe <- doesFileExist fp
  TOD ft 0 <- if fe then getModificationTime fp else return $ TOD 0 0
  if isSourceFile fp && ft > t then return True else do
  de <- doesDirectoryExist fp
  if (not de) then modSince t fps else do
  todo <- return . map ((fp++"/")++) . filter (/=".") . 
          filter (flip elem "ABCDEFGHIJKLMNOPQRSTUVWXYZ_" . head) . 
          filter ((not . flip elem "._") . head)
              =<< getDirectoryContents fp
  contSince <- modSince t todo
  if contSince then return True else modSince t fps

     

     
    
isSame a b = do
  (inh,outh,errh,p) <- runInteractiveCommand $ unwords ["diff","-Nq",a,b]
  return . null =<< hGetContents outh

ls = sys ["ls"]
rm fp = sys ["rm",fp]
move a b  = sys ["mv",a,b]

runProgram binary args =  do
  exe <- canonicalizePath binary
  print (exe:args) 
  runProcess exe args Nothing Nothing Nothing Nothing Nothing

data ModLoc = ModDir FilePath
            | ModURI MapDir ModBase URI -- becomes -i MapId if not already specific
            | ModTGZ  MapDir ModBase URI (Maybe BuildCmd) FilePath
            | ModRepo MapDir ModBase RepoCmd FilePath -- FilePath is relative to location of repocmd

              deriving Show

data MapLoc = MapDir FilePath
            | MapFile FilePath
            | MapURI URI deriving Show

type ModBase = [String]
type MapDir= FilePath
type RepoCmd = String
type BuildCmd = String
type URI = String

{-- take command line options and turn them into a configuation --}
data Conf = Conf {hsFiles::[FilePath] -- if file not found on current path then pulls from net
                 ,modLocs::[ModLoc]
                 ,mapLocs::[MapLoc]
                 ,cargs::[String]
                 ,cacheDir::String
                 ,exe::String
                 ,maxAge::Integer -- how long in seconds since files were retrieved
                 ,start::Integer
                 ,verbose::Int
                 } deriving Show

temp_targ targ = targ ++ ".sp.new"
confToCmdLine conf = concat $ intersperse " " $
   (exe conf):toI conf ++ (hsFiles conf) ++ (cargs conf) 

toI conf = map impl mods
    where
    mods = map head $ groupBy modMatch $ modLocs conf
    modMatch (ModURI m1 _ _) (ModURI m2 _ _) = m1==m2
    modMatch (ModTGZ m1 _ uri1 c1 b1) (ModTGZ m2 _ uri2 c2 b2) = 
        (m1,uri1,c1,b1)==(m2,uri2,c2,b2)
    modMatch _ _ = False
    impl (ModDir fp) = "--includes="++fp
    impl (ModURI mapdir _ _ ) = "--includes="++mapdir
    impl (ModRepo mapdir _ repoCmd baseDir) = 
                         "--includes="++mapdir++"/"++(escapeURI repoCmd) ++ "/" ++ baseDir
    impl (ModTGZ mapdir _ uri _ baseDir) =
        "--includes="++mapdir++"/"++(escapeURI uri) ++ "/" ++ baseDir

getMapDirs conf = [dir | MapDir dir <- mapLocs conf]

defaultConf = Conf {hsFiles=[],mapLocs=[],modLocs=[],cacheDir=".sp-cache",
                    maxAge=3600*24*14,exe="",cargs=[],start=0
                   ,verbose=0
                   }

prepConf conf [] = do
  createMissingDir (cacheDir conf)
  now <- getClockTime
  let TOD secs _ = now
  dirAge <- getModificationTime (cacheDir conf) >>= (return . tdSec . diffClockTimes now)
  return conf {hsFiles = reverse $ hsFiles conf
              ,mapLocs = (reverse $ mapLocs conf) 
              ,cargs = reverse $ cargs conf
              ,maxAge = max 120 $ maxAge conf ---min dirAge $ maxAge conf
              ,start=secs
              }

prepConf conf ("--sp-verbose":d:args) = prepConf conf {verbose=read d} args

prepConf conf (('-':'i':path'):args') = do
  let (path:args)=if null $ words path' then args' else (path':args')
  exist <- doesFileExist path
  isDir <- doesDirectoryExist path
  if isDir then prepConf conf {mapLocs = MapDir path:mapLocs conf} args else do 
  if exist then prepConf conf {mapLocs = MapFile path:mapLocs conf} args else do
  prepConf conf {mapLocs = MapURI path:mapLocs conf} args                                                                           

prepConf conf ("--maxAge":t:args) = prepConf conf {maxAge=read t}  args
prepConf conf ("--max-age":t:args) = prepConf conf {maxAge=read t}  args

prepConf conf (arg:args) = prepConf'
  where
  prepConf' 
    | null $ exe conf = prepConf conf {exe=arg} args
    | not ("run" `isPrefixOf` (exe conf))
      && isSourceFile arg = prepConf conf {hsFiles=arg:hsFiles conf} args
    | isSourceFile arg && (null $ hsFiles conf) = prepConf conf {hsFiles = arg:hsFiles conf} args
    | otherwise = prepConf conf {cargs = arg:cargs conf} args
                                                                                    
sourceExts = words ".hs .lhs .hi .ycr"
isSourceFile path = any (`isSuffixOf` path) sourceExts 


-----------------
getModLocs conf (MapDir fp) = return [ModDir fp]
getModLocs conf (MapFile filePath) = 
    getModLocs' (mapDir conf filePath) filePath
getModLocs conf (MapURI uri) = do
  let fp = mapFile conf uri 
  getURI conf uri fp 
  fe <- doesFileExist fp
  if fe then getModLocs' (mapDir conf uri) fp else do
  hPutStr stderr errMsg
  return []
  where
  errMsg = "\nWarning: Searchpath could not use curl to retrieve uri "++ uri++"\n"++
           "Either curl is not configured properly on this machine or the uri is not avilable at the moment.\n\n"
getModLocs' mapdir filePath = do
  file <- readFile' filePath >>= 
          return . unlines . --filter (\x->(head $ ltrim x) /= '#' ) .
                 filter (not . null) . map (ltrim . takeWhile (/='#')) . lines
  return $ map (toModLoc . words) $ lines file
  where
  toModLoc::[String] -> ModLoc
  toModLoc (modbase':rest)
      | len == 1 = ModURI mapdir modbase (head rest)
      | len == 2 = ModTGZ mapdir modbase (head rest) Nothing pos2
      | head pos2=='"' = -- seq (error $ "AAA" ++ show rest) $
          ModTGZ mapdir modbase (head rest) buildCmd (last rest)
      | otherwise = ModRepo mapdir modbase repoCmd (last rest)
      where len = length rest
            pos2 = (head $ tail rest)
            modbase = words $ tr '.' ' ' $ modbase'
            buildCmd = Just $ read $ r $ 
                       drop 1 rest -- modbase and tgz url
            r x = unwords $ init $ x
            repoCmd = case read (unwords $ init rest) of
                        s | head (words s) `elem` vcs -> s
                          | otherwise -> "echo UNKNOWN VC: "++show s
vcs = words "darcs ln svn cvs arch svk git bzr mercurial rcs cp curl sp configure make tar" ++
      words "bzip2 echo runhaskell runghc runhugs cabal"

{--
File format is
  Module.Base http://base/uri #comment 
  #comment

  # blanklines allowed
  Module.Base "darcs or svn or cvs command" rel/path/from/command

  Mos.Base http://foo/bar.tgz rel/path/from/extract/tgz
--}

ltrim::String->String
ltrim = dropWhile (flip elem " \t\r")   
mapFile conf path = cacheDir conf ++ '/':escapeURI path 
mapDir conf path = cacheDir conf
    -- mapFile conf path ++ ".dir"


escapeURI = tr ';' '@' .
            tr '%' '@' . escapeURIString (not.flip elem "?:/\\'\"%") .
            tr '/' '-' . tr ' ' '_' . replace "://" "_" . unwords . words
               
tr a b list = map (\x->if x==a then b else x) list
replace _ _ [] = []
replace a b list@(h:rest) = if isPrefixOf a list then b ++ drop (length a) list
                            else h:replace a b rest

----------------------------
createMissingDir dir = do
  de <- doesDirectoryExist dir
  unless de $ do
    createDirectoryIfMissing True dir

getDir path = reverse $ dropWhile (/='/') $ reverse path

defaultModificationTime fp = do
  fe <- doesFileExist fp
  if fe then getModificationTime fp else return (TOD 0 0)

mbFileExists fp = do
  --de <- doesDirectoryExist fp
  fe <- doesFileExist fp
  if fe then return $ Just fp else return Nothing

getURI :: Conf -> URI -> FilePath -> IO (Maybe FilePath)
getURI c u p = getURI' c u "" p

getURI' :: Conf -> URI -> String -> FilePath -> IO (Maybe FilePath)
getURI' conf url proxy path =
    do
    createMissingDir dir
    hPutStr stderr "<"
    TOD now _ <- getClockTime
    TOD mt _ <- defaultModificationTime path
    TOD urlMT _ <- defaultModificationTime urlFP -- negative caching
    let current = now - (max mt urlMT) < maxAge conf
    unless current $ getImpl
    mbFileExists path
    where
    dir = getDir path
    urlFP = dir ++ "/" ++ escapeURI url
    getImpl = do
      let pxy = if (length proxy > 0) then ["-x", proxy] else []
          cmd = (["curl"] ++ words "-A curl-searchpath/yhcws -f -s -L " ++ pxy ++
                       [url, "-o", path])
          cml = concat (intersperse " " cmd)
      debug 3 conf "CURL" $ print cml
      ret <- rawSystem (head cmd) (tail cmd)
      case ret of ExitSuccess -> hPutStr stderr ">" 
                  ExitFailure 22 -> writeFile urlFP "" >> --negative cache
                                    hPutStr stderr " " -- 4xx, 5xx
                  _              -> hPutStr stderr "!"

modListify = words . tr '.' ' ' . dropSuffix
dropSuffix path = if '.' `elem` path then reverse $ tail $ dropWhile (/='.') $ reverse path else path

moduleChase done conf modLocs hsFile = do
  fe <- doesFileExist hsFile
  hsFile' <- if fe then return $ Just hsFile else do
             isTodo conf modLocs $ modListify  hsFile
  --when (not fe) $ error $ "not handling remote hsfiles yet " ++ hsFile
  imports <- getFileImports hsFile >>= return . 
             filter (not . flip Set.member done) .
             map (words . tr '.' ' ') . unique
  let done' = Set.union done $ Set.fromList imports

  --convert imports to see if they are in the base and then find in any of those locations if not try retrieving
  todo <- mapM (isTodo conf modLocs) imports >>=  return . map fromJust . filter isJust
  (done'',_) <- foldM (\(done,_) hsFile-> do
                       (d2,_) <- moduleChase done conf modLocs hsFile
                       return $ (Set.union done d2,hsFile)
                    )
              (done',hsFile) todo 
  
  return (done'',hsFile')  -- so we can handle remote files!


mbSumIO f = foldl (\mbVal item->maybe (f item) (return . Just) =<< mbVal) (return Nothing)
unique = map head . group . sort

isTodo conf modlocs imp = mbSumIO isTodo'' modlocs
  where 
  slashed ="/"++(concat $ intersperse "/" imp) 
  isTodo'' mod = do x<-isTodo' mod
                    debug 2 conf "ISTODO?" $ print mod >> print x
                    return x
  isTodo' (ModDir dir) = do
    let fp = dir ++ slashed
    return . msum =<< mapM (mbFileExists . (fp++) ) sourceExts 

  isTodo' (ModURI mapdir ms uri) = do
    if not $ ms `isPrefixOf` imp then return Nothing else do
    mbSumIO (getFile . (slashed++)) sourceExts
    where
    getFile slashed = do
      let fp = mapdir ++ slashed::String
      getURI conf (uri++slashed) fp

  isTodo' mod@(ModTGZ mapdir ms uri mbBuild baseDir) = do
    if not $ ms `isPrefixOf` imp then return Nothing else do

    cwd <- getCurrentDirectory
    let repodir = mapdir ++ '/':escapeURI uri
        tgz = repodir ++ "/" ++ escapeURI uri
    fe <- doesFileExist tgz
    TOD t1 _ <- if fe then getModificationTime tgz else return $ TOD 0 0
    mb <- getURI conf uri tgz
    debug 1 conf ("Retrieved " ++ uri) $ print mb
    if isNothing mb then return Nothing else do
    TOD t2 _ <- getModificationTime tgz 
    when (t2-t1> maxAge conf) $ do
      setCurrentDirectory repodir
      let cmd = "tar -xzf "++escapeURI uri
      getCurrentDirectory >>= print
      print cmd
      sys $ words cmd
      when (isJust mbBuild) $ do (doCmds $ fromJust mbBuild) >> return ()
      setCurrentDirectory cwd                                         
    let fp = repodir ++ "/" ++ baseDir ++ slashed
    mapM (mbFileExists . (fp++)) sourceExts >>= return . msum
  
  isTodo' (ModRepo mapdir ms repoCmd baseDir) = do
    if not $ ms `isPrefixOf` imp then return Nothing else do
    cwd <- getCurrentDirectory
    let repodir = mapdir ++ '/':escapeURI repoCmd
    de <- doesDirectoryExist repodir
    -- !!! handle if we should try to wipe and pull this repo again?
    unless de $ do
      createMissingDir repodir
      setCurrentDirectory repodir
      print repoCmd
      doCmds repoCmd

      --sys $ words repoCmd -- !!!! handle failure!?

      setCurrentDirectory cwd
    let fp = repodir ++ "/" ++ baseDir ++ slashed
    mapM (mbFileExists . (fp++)) sourceExts >>= return . msum
  toCmds c = filter (\x->head x `elem` vcs) $ map words $ lines $ tr ';' '\n'c
  doCmds = mapM sys . toCmds
readFile' x = catch (readFile x) (\_->return "")

getFileImports fileName = fmap (getImports isLit) (readFile' fileName)
    where
    isLit = ".lhs" `isSuffixOf` fileName

moduleRE = mkRegex "^[ \t]*module[ \t]+([^ \t\n\r]+).*$"
importRE = mkRegex "^[ \t]*import[ \t]+(qualified[ \t]+)?([^()\n\r \t]+).*$"

type ModuleName = String
getImports :: Bool -> String -> [ModuleName]
getImports isLit = altParse isLit 1 importRE

altParse :: MonadPlus m => Bool -> Int -> Regex -> String -> m ModuleName
altParse isLit ex re
 = msum .
   fmap (maybe mzero (return . (!! ex)) . matchRegex re) .
   parseLines isLit
fnMap fs x = fmap (\f->f x) fs
--parseLines :: String -> [String]
parseLines isLit = fmap stripComments . concatMap lines . fnMap [id, unLit isLit,unLatex]

--unLit :: String -> String
unLit True src = unlines $ map tail $ filter (isPrefixOf ">") $ lines src
unLit _ src = src

unLatex :: String -> String
unLatex src = impl id src
    where
    impl code src
        | null src = code ""
        | isPrefixOf beginCode src =
            impl (\x->code $ newCode ++ x) (drop codeLen src)
        | otherwise = impl code (tail src)
    codeLen = untilPrefix endCode 0 $ drop lenBeginCode src
    newCode = take codeLen $ drop lenBeginCode src
    beginCode="\\begin{code}"
    lenBeginCode = length beginCode
    endCode = "\\end{code}"

stripComments :: String -> String
stripComments src = impl "" src
    where impl code src
              | null src = reverse code
              | isPrefixOf "{-" src = impl code $ after "-}" src
              | isPrefixOf "--" src = impl code $ after "\n" src
              | otherwise = impl (head src:code) (tail src)

untilPrefix prefix size [] = size
untilPrefix prefix size src = if isPrefixOf prefix src then size
                        else untilPrefix prefix (size+1) $ tail src

after prefix [] = []
after prefix src = if isPrefixOf prefix src then drop (length prefix) src
                   else after prefix $ tail src
