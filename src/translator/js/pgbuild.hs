-- An experimental utility to build a (X)HTML page out of a
-- template and a set of scripts to embed and/or to make referred to
-- from the resulting page.

module Main where

import Data.Maybe
import Network.URI
import Control.Monad
import Text.XML.HXT.Parser.MainFunctions
import Text.XML.HXT.DOM.XmlKeywords
import Text.XML.HXT.DOM.FormatXmlTree
import Text.XML.HXT.DOM.XmlTreeFilter
import Text.XML.HXT.DOM.XmlTreeFunctions
import qualified Data.Tree.NTree.Filter as DTNF
import System.Environment
import System.Console.GetOpt
import System.FilePath
import System.Posix.Files
import System.Exit
import System.IO

-- Data type to encode command line options.

data PkgArg = Verbose
            | OutPath String
            | TmplPath String
            | HelpMsg
            | ScrURI String
            | ScrPath String
            | PgTitle String
            | PgOnload String
            deriving (Eq, Show)

-- Control structure for getOpt.

pkgOpt :: [OptDescr PkgArg]

pkgOpt = [
  Option ['v']  ["verbose"]         (NoArg Verbose)       "provide verbose output of XML tree",
  Option ['t']  []                  (ReqArg TmplPath "")  "specify page template file location",
  Option ['o']  []                  (ReqArg OutPath "")   "specify output file location",
  Option ['e']  []                  (ReqArg ScrPath "")   "script file path follows",
  Option ['u']  []                  (ReqArg ScrURI "")    "script URI follows",
  Option ['T']  ["title"]           (ReqArg PgTitle "")   "set page title",
  Option []     ["onload"]          (ReqArg PgOnload "")  "set <body onload=> attribute",
  Option ['?','h'] ["help"]         (NoArg HelpMsg)       "print this help message"
  ]

-- Parse the command line options. If nothing on the command line
-- then just print the help message.

parseOpt argv = 
  case getOpt Permute pkgOpt argv of
    (o,n,[]  ) -> if ((length o) + (length n) == 0) 
                    then do putStrLn $ usageInfo header pkgOpt
                            return ([],[])
                    else if HelpMsg `elem` o
                           then do putStrLn $ usageInfo header pkgOpt
                                   return ([],[])
                           else return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header pkgOpt))
  where header = "Usage: pgbuild [OPTION...] script... \n" ++
                 "  where script may be a path (to embed), or an URI (to refer).\n" ++
                 "  Both template and output files must be specified, and must not be the same"

-- Data type for processed options and file paths.

data OptInfo = OptInfo {
  outPath  :: Maybe FilePath,
  tmplPath :: Maybe FilePath,
  scrLocs  :: [Either String String],       -- Left: embeddable, Right: URI
  badURIs  :: [String],
  pgTitle  :: String,                       -- empty string leaves title as is
  pgOnload :: String,
  beVerbose :: Bool
  
} deriving (Show)

defaultOptInfo :: IO OptInfo

defaultOptInfo = return OptInfo {outPath = Nothing,
                                 tmplPath = Nothing,
                                 beVerbose = False,
                                 pgTitle = "",
                                 pgOnload = "",
                                 badURIs = [],
                                 scrLocs = []}

-- Update the OptInfo record from a parsed option.

updOptInfo :: OptInfo -> PkgArg -> OptInfo  

updOptInfo oi Verbose      = oi {beVerbose = True}
updOptInfo oi (OutPath s)  = oi {outPath = Just s}
updOptInfo oi (TmplPath s) = oi {tmplPath = Just s}
updOptInfo oi (ScrPath s)  = oi {scrLocs = (scrLocs oi) ++ [Left s]}
updOptInfo oi (ScrURI s)   = 
  if (isAbsoluteURI s || isRelativeReference s)
    then oi {scrLocs = (scrLocs oi) ++ [Right s]}
    else oi {badURIs = (badURIs oi) ++ [s]}
updOptInfo oi (PgTitle s)  = oi {pgTitle = s}
updOptInfo oi (PgOnload s) = oi {pgOnload = s}
updOptInfo oi _            = oi

-- Update the default options from the options parse result.
-- All non-options are embeddable script paths.

updOptions :: ([PkgArg], [String]) -> OptInfo -> OptInfo

updOptions (op, nop) oi = (upd2 op) (oi {scrLocs = map Left nop}) where
  upd2 [] oi = oi
  upd2 (o:os) oi = (upd2 os) (updOptInfo oi o)

-- The main function: parse the arguments, parse the template,
-- embed scripts/URIs, dump the resulting page.

main = do
  opts <- getArgs >>= parseOpt
  dopt <- defaultOptInfo >>= (return . (updOptions opts))

-- Validate input parameters.

  when ((length $ badURIs dopt) > 0) $ do
    mapM (hPutStrLn stderr . \s -> "pgbuild: " ++ show s ++ " is not a valid URI") (badURIs dopt)
    exitWith (ExitFailure 1)

  when (outPath dopt == Nothing || tmplPath dopt == Nothing) $ do
    hPutStrLn stderr "pgbuild: template and/or output file path not provided"
    exitWith (ExitFailure 2)

-- Make sure the template and output files are not the same:
-- use FileStatus.

  let eifs p = catch (getFileStatus p >>= return . Right) (return . Left)

  [eif1, eif2] <- mapM (eifs . fromJust) [outPath dopt, tmplPath dopt]

  case eif2 of
    (Left err) -> do
      hPutStrLn stderr $ "pgbuild: " ++ show err
      exitWith (ExitFailure 3)
    (Right fs2) -> case eif1 of
      (Left _) -> return ()
      (Right fs1) -> when (fileID fs1 == fileID fs2 && deviceID fs1 == deviceID fs2) $ do
        hPutStrLn stderr $ "pgbuild: " ++ fromJust (outPath dopt) ++ " and " ++ 
                           fromJust (tmplPath dopt) ++ " are identical"
        exitWith (ExitFailure 4)

-- For each embeddable script, read its contents and replace path to a script
-- with script contents.

  let getScript u@(Right _) = return u
      getScript   (Left p) = catch (do
        h <- openFile p ReadMode
        (hGetContents h) >>= (return . Left)
        ) (\e -> hPutStrLn stderr ("pgbuild: " ++ show e) >> exitWith (ExitFailure 5))

  locs' <- mapM getScript (scrLocs dopt)

  dopt' <- return dopt {scrLocs = locs'}

-- Read and parse the template file.

  let attrs = [(a_parse_html, "1"),
               (a_canonicalize, "0"),
               (a_output_xml, "0"),
               (a_encoding,   utf8)]
              
  (res, err, rc) <- getXmlDocument attrs (fromJust $ tmplPath dopt')

-- Process the tree from the top down.
-- If a <title> tag is found, and title replacement was specified, replace the title
-- If a <head> tag is found and list of scripts is not empty, add <script> tags
-- at the end if its children list.
-- If a <body> tag is found, set onload to what was provided on the command line

  let res' = (DTNF.processTopDown pgFilter1 `DTNF.o`
              DTNF.processTopDown pgFilter2 `DTNF.o`
              DTNF.processTopDown pgFilter3) res
      pgFilter1 = modifyTitle (pgTitle dopt') `DTNF.when` isTag "title"
      pgFilter2 = appendScripts (scrLocs dopt') `DTNF.when` isTag "head"
      pgFilter3 = setOnload (pgOnload dopt') `DTNF.when` isTag "body"

      modifyTitle s = DTNF.processChildren (modifyText (\x -> if (length s) > 0 then s else x))
      appendScripts ss = DTNF.insertChildrenAfter DTNF.this (mkScripts ss)
      setOnload onl = if (length onl) == 0 
                        then DTNF.this
                        else DTNF.iff (hasAttr "onload")
                                      (modifyAttr "onload" (\x -> onl))
                                      (addAttr "onload" onl)
      mkScripts [] = DTNF.none
      mkScripts (sc:scs) =  mkScript sc DTNF.+++ mkScripts scs
      mkScript (Right uri) = mkXTag "script" ((mkXAttr "src" (mkXText uri)) DTNF.+++ scrType)
                                             DTNF.none
      mkScript (Left  scr) = mkXTag "script" scrType 
                                             (mkXText scr)
      scrType = mkXAttr "type" (mkXText "text/javascript")

  when (beVerbose dopt) $ mapM_ (putStrLn . formatXmlTree) res'

  mapM (putXmlDocument attrs (fromJust $ outPath dopt)) res'


