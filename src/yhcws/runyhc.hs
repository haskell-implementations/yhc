-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Dmitry Golubovsky, 2007       
-- License     :  BSD-style
-- 
-- Maintainer  :  Dmitry Golubovsky <golubovsky@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- YHC Web Service: Find a new document in the pastebin, run compilation,
-- upload results as attachments to this document.
--
-- This program polls the "pastebin" database for documents with the "status"
-- field containing "New". Source code (the "source" field) is extracted 
-- from the document, saved in a file, and Yhc is run to compile it into HTML.
-- Compilation/error log, the HTML page and the stylesheet are saved with 
-- the original document as attachments.
--
-----------------------------------------------------------------------------

module Main where

import Data.Maybe
import Database.CouchDB
import Text.JSON
import Control.Monad
import System.Environment
import System.FilePath
import System.Posix.Files
import System.Posix.Process
import System.Exit
import System.IO
import Network.URI
import Data.Time
import System.Locale
import System.Directory
import System.Process
import System.Cmd
import Control.Exception
import qualified Data.ByteString.Char8 as C

pb = "pastebin"

curltmpl = "curl -s -f -L -x http://localhost:5984 " ++
           "http://localhost:5984/static/yhcws/template.html -o " ++
           "template.html"

main = do
  prog <- getProgName
  args <- getArgs
  when (length args == 0) $ do
    hPutStrLn stderr $ "Usage: " ++ prog ++ " couchdb_uri"
    exitWith (ExitFailure 2)
  let puri = fromMaybe nullURI $ do
        baseuri <- parseAbsoluteURI (head args)
        pburi <- parseURIReference ("/" ++ pb ++ "/")
        pu <- pburi `relativeTo` baseuri
        return pu
  mapM (\(s, u) -> when (u == nullURI) $ do
    hPutStrLn stderr $ prog ++ ": could not parse the database uri " ++ s
    exitWith (ExitFailure 3)) $ zip [head args ++ "/" ++ pb] 
                                    [puri]
  pjsn <- cDBGet puri >>= return . rsp2CDB >>= cdb2JSON
  dbname <- getValueByName "db_name" pjsn >>= getString
  when (dbname /= pb) $ do
    hPutStrLn stderr $ prog ++ ": database name " ++ dbname ++ " was not expected for pastebin"
    exitWith (ExitFailure 4)
  let askone = parseRelativeReference "?count=1"
      viewq = "function(doc) {if (doc.status=='New') {map(null,!!doc._id?doc._id:'0');}}"
  newdoc <- cDBTempView puri askone viewq 
              >>= return . rsp2CDB >>= cdb2JSON
              >>= getValueByName "rows" >>= getColumn "value" >>= mapM getString
  when (null newdoc) $ do
    exitWith (ExitFailure 99)
  let nduri = fromJust (dbDocURI puri (head newdoc))
  doc <- cDBGet nduri >>= return . rsp2CDB >>= cdb2JSON
  src <- getValueByName "source" doc >>= getString
  ttl <- getValueByName "title"  doc >>= getString
  let doc' = doc `mergeL` ("status", String "Compiling")
  expath <- getProcessID 
        >>= \p -> readSymbolicLink ("/proc" </> show p </> "exe") 
        >>= return . takeDirectory
  rsp <- cDBPutJSON nduri doc' >>= return . rsp2CDB >>= cdb2JSON
  when (hasValueWithName "error" rsp) $ do
    hPutStrLn stderr $ prog ++ ": could not reset the document status"
    exitWith (ExitFailure 127)
  rev <- getDocRevision rsp
  did <- getDocID rsp
  let wrkdir = did <.> rev
  de <- doesDirectoryExist wrkdir
  when de $ do
    hPutStrLn stderr $ prog ++ ":the document " ++ wrkdir ++ " is being worked on"
    exitWith (ExitFailure 99)
  bracket_ (createDirectory wrkdir) 
           (updateDoc prog doc' nduri rev did wrkdir >> rawSystem "rm" ["-rf", wrkdir]) $ do
    C.writeFile (wrkdir </> "error_log")
                (C.pack $ "* This is an error log placeholder. If you see it, \n" ++
                          "* uncaught exception occurred during compilation\n")
    src <- getValueByName "source" doc' >>= getString
    let srcb = C.pack src
        modn = getModName src
        pgms = getPragmas src
        htmpl = getTemplate pgms
        maps = getMaps pgms
        mkfile = C.pack (mkMakefile modn expath htmpl maps)
    when (null modn) $ do 
      C.writeFile (wrkdir </> "error_log") 
                  (C.pack $ prog ++ ": document does not contain valid Haskell module name\n")
      error $ "document " ++ did ++ "does not contain valid Haskell module name"
    C.writeFile ("." </> wrkdir </> modn <.> "hs") srcb
    C.writeFile ("." </> wrkdir </> "Makefile") mkfile
    (si, so, se, pid) <- runInteractiveProcess 
      "make" [] (Just $ "./" ++ wrkdir) Nothing 
    mkout <- hGetContents so
    mkerr <- hGetContents se
    rc <- waitForProcess pid
    case rc of
      ExitSuccess -> do
        let loghd = C.pack $ "* Compilation of " ++ modn ++ " was successful\n" ++
                             "* Compile log follows\n\n"
            logbd = C.pack mkout
        C.writeFile (wrkdir </> "compile_log") (C.concat [loghd, logbd])
        rawSystem "cp" [wrkdir </> modn <.> "html", wrkdir </> "webpage.html"]
        return ()
      ExitFailure ec -> do
        let loghd = C.pack $ "* Compilation of " ++ modn ++ 
                             " failed with code " ++ show ec ++ "\n" ++
                             "* Error log follows\n\n"
            logbd = C.pack $ mkerr ++
                             "\n* Standard output of the compilation process follows\n\n" ++
                             mkout
        C.writeFile (wrkdir </> "error_log") (C.concat [loghd, logbd])
        return ()
  exitWith ExitSuccess

mkMakefile modn spath tmpl maps =
  "all: template.html " ++ modn ++ ".html\n" ++
  "YHCBASE := $(shell dirname $(shell dirname $(shell which yhc)))\n" ++
  "include $(YHCBASE)/lib/mk/HsWebPage.mk\n" ++
  "XMLTMPL = template.html\n" ++
  "%.yca: %.hs\n\t" ++ (spath </> "spyhc") ++ 
  " -i. -i$(YHCBASE)/lib/yws -i$(HSJSLIB) " ++ extmaps ++ 
  " yhc --cpp --linkcore --no-bytecode $<\n\n" ++
  "template.html:\n\t" ++
  case tmpl of
    Nothing -> curltmpl
    Just t -> "curl -s -f -L " ++ (show t) ++ " -o $@"
  where extmaps = concat $ map (\i -> " -i" ++ show i ++ " ") maps

updateDoc :: String -> JsonNode -> URI -> String -> String -> FilePath -> IO ()

updateDoc prog doc uri rev did wrkdir = do
  let tx8 = "text/plain;charset=utf-8"
      th8 = "text/html;charset=utf-8"
      epth = wrkdir </> "error_log"
      cpth = wrkdir </> "compile_log"
      wpge = wrkdir </> "webpage.html"
  clog <- doesFileExist cpth
  elog <- doesFileExist epth
  pgxt <- doesFileExist wpge
  if elog && (not clog)
    then do
      el <- C.readFile epth
      let eatt = binAttNode "error_log" tx8 el
          doc' = doc `mergeL` ("status", String "Error")
                     `mergeL` ("_rev", String rev)
                     `mergeL` ("_id", String did)
                     `mergeL` ("_attachments", eatt)
      rsp <- cDBPutJSON uri doc' >>= return . rsp2CDB >>= cdb2JSON
      when (hasValueWithName "error" rsp) $ do
        hPutStrLn stderr $ prog ++ ": could not reset the document status"
        exitWith (ExitFailure 127)
      return ()
    else do
      cl <- if clog then C.readFile cpth else return $ C.pack ""
      pg <- if pgxt then C.readFile wpge else return $ C.pack ""
      let clatt = binAttNode "compile_log" tx8 cl
          pgatt = binAttNode "webpage.html" th8 pg
          doc' = doc `mergeL` ("status", String "Success")
                     `mergeL` ("_rev", String rev)
                     `mergeL` ("_id", String did)
                     `mergeL` ("_attachments", clatt `unionL` pgatt)
      rsp <- cDBPutJSON uri doc' >>= return . rsp2CDB >>= cdb2JSON
      when (hasValueWithName "error" rsp) $ do
        hPutStrLn stderr $ prog ++ ": could not reset the document status"
        hPutStrLn stderr $ show rsp
        exitWith (ExitFailure 127)
      return ()

data YWS_PRAGMA = 
     YP_SP_MAP URI
   | YP_TEMPLATE URI

getPragmas :: String -> [YWS_PRAGMA]

getPragmas src = 
  let lns = map words (lines src)
      fltp ["{-#", pgmn, pgmv, "#-}"] = case pgmn of
        "SP_MAP" -> case parseAbsoluteURI pgmv of
           Nothing -> []
           Just u -> [YP_SP_MAP u]
        "HTML_TEMPLATE" -> case parseAbsoluteURI pgmv of
           Nothing -> []
           Just u -> [YP_TEMPLATE u]
        _ -> []
      fltp _ = []
  in  concat $ map fltp lns

getTemplate :: [YWS_PRAGMA] -> Maybe URI

getTemplate [] = Nothing
getTemplate ((YP_SP_MAP _) : r) = getTemplate r
getTemplate ((YP_TEMPLATE t) : _) = Just t

getMaps :: [YWS_PRAGMA] -> [URI]

getMaps pms = concat $ map onemap pms
  where onemap (YP_SP_MAP m) = [m]
        onemap _ = []

getModName :: String -> String

getModName src = 
  let src' = map pn2spc src
      pn2spc '(' = ' '
      pn2spc z = z
      lns = map words (lines src')
      fltm ("module":mdn:_) = True
      fltm _ = False
      stmod = dropWhile (not . fltm) lns
      modn = case stmod of
        [] -> ""
        ("module":mdn:_):_ -> mdn
        _ -> ""
  in  modn

