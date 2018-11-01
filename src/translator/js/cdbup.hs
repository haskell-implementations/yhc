-- An utility to upload a generated HTML page into CouchDB as an attachment

module Main where

import Data.Maybe
import Database.CouchDB
import Text.JSON
import Control.Monad
import System.Environment
import System.Console.GetOpt
import System.FilePath
import System.Posix.Files
import System.Exit
import System.IO
import Network.URI
import qualified Data.ByteString as B

-- Data type to encode command line options.

data PgmArg = Verbose
            | DocRev String
            | ContType String
            | NewDoc
            | ModDoc
            | BinAtt
            | HelpMsg
            | PrintRev
            | UsePost
            deriving (Eq, Show)

-- Control structure for getOpt.

pgmOpt :: [OptDescr PgmArg]

pgmOpt = [
  Option ['v'] ["verbose"]      (NoArg Verbose)
    "print additional information to stderr"
 ,Option ['r'] ["revision"]     (ReqArg DocRev "")
    "modify specific revision of the document"
 ,Option ['n'] ["new-document"] (NoArg NewDoc)
    "create a new document, fail if already exists. -n supersedes -m and -r"
 ,Option ['m'] ["modify"]       (NoArg ModDoc)
    "modify the latest or specified (with -r) revision of the document"
 ,Option ['b'] ["binary"]       (NoArg BinAtt)
    "assume binary attachment"
 ,Option []    ["print-rev"]    (NoArg PrintRev)
    "print DocID and the revision of the new/updated document"
 ,Option ['c'] ["content-type"] (ReqArg ContType "")
    ("specify content type of the attachment: \n" ++
     "default is text/plain, however \n" ++
     "-b changes default to application/octet-stream")
 ,Option [] ["post"]            (NoArg UsePost)
    ("use POST to create a document with server-assigned DocID.\n" ++
     "This option forces --print-rev. Supply database URL \n" ++
     "instead of document URL. --post supersedes -n, -m, and -r")
 ,Option ['?', 'h'] ["help"]    (NoArg HelpMsg)
    "print this help message"]

-- Parse the command line options. If nothing on the command line
-- then just print the help message.

parseOpt argv = 
  case getOpt Permute pgmOpt argv of
    (o,n,[]  ) -> if ((length o) + (length n) == 0) 
                    then do putStrLn $ usageInfo header pgmOpt
                            return ([],[])
                    else if HelpMsg `elem` o
                           then do putStrLn $ usageInfo header pgmOpt
                                   return ([],[])
                           else return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header pgmOpt))
  where header = "Usage: cdbup [OPTION...] document_url attachment_path \n" ++
                 "  Create new or update an existing document with an attachment.\n"


-- Validate options and arguments. There should be >= 2 arguments 
-- (may be more, but more than 2 will be ignored). The first argument
-- should be parseable absolute URI, the second should be a valid FilePath.
-- If NewDoc is in options, ModDoc and DocRev will be discarded. If UsePost
-- is in options, PrintRev will be added.

validateOpts :: [PgmArg] -> [String] -> ([String], URI, FilePath, [PgmArg])

validateOpts opts args = (emsgs, baseuri, fp, opts''') where
  emsgs  | HelpMsg `elem` opts = []
         | otherwise = argerr ++ fperr ++ urierr
  argerr | (length args) >= 2  = []
         | otherwise           = ["Program should be called with two non-option arguments"]
  (ur:fp:_) = args ++ (cycle [""])
  fperr  | isValid fp          = []
         | otherwise           = ["Attachment path " ++ fp ++ " is not valid"]
  baseuri' = parseAbsoluteURI ur
  urierr | baseuri' == Nothing = ["Document/database URI " ++ ur ++ " is not valid"]
         | otherwise           = []
  baseuri = fromMaybe nullURI baseuri'
  f1 ModDoc       = True
  f1 (DocRev _)   = True
  f1 _            = False
  f2 PrintRev     = True
  f2 NewDoc       = True
  f2 z            = f1 z
  f3 (ContType _) = True
  f3 _            = False
  opts'   | NewDoc `elem` opts = filter (not . f1) opts
          | otherwise = opts
  opts''  | UsePost `elem` opts' = PrintRev : filter (not . f2) opts'
          | otherwise = opts'
  opts''' | BinAtt `elem` opts'' && filter f3 opts'' == [] 
              = (ContType "application/octet-stream") : opts''
          | BinAtt `notElem` opts'' && filter f3 opts'' == []
              = (ContType "text/plain") : opts''
          | otherwise = opts''

main = do 
  (opts0, args0) <- getArgs >>= parseOpt
  when (HelpMsg `elem` opts0) $ exitWith ExitSuccess
  let (erms, uri, apth, opts) = validateOpts opts0 args0
  when (length erms /= 0) $ do
    mapM_ putStrLn erms
    exitWith (ExitFailure 2)

-- Obtain the database information to make sure it exists.

  when (Verbose `elem` opts) $ do
    hPutStrLn stderr "Obtaining the database or document information ..."
  cdb <- cDBGet uri >>= return . rsp2CDB
  when (Verbose `elem` opts) $ do
    hPutStrLn stderr "Response:"
    hPutStrLn stderr $ showPP cdb
  let infoerr c u = "HTTP code " ++ show c ++ " while accessing " ++ show u
  dbjs <- case cdb of
    CDBJSON 200 _ _ -> 
      if (NewDoc `elem` opts) then error "Document already exists" else cdb2JSON cdb
    CDBJSON 404 _ _ -> 
      if (NewDoc `elem` opts) then cdb2JSON cdb else error $ infoerr 404 uri
    CDBJSON c _ _ -> error $ infoerr c uri
    CDBHTTP c r -> error $ infoerr c uri
    CDBBODY c r -> error $ infoerr c uri

-- Build the attachment node.

  let fct (ContType _) = True
      fct _            = False
      (ContType ctype) = head $ filter fct opts
      fname = takeFileName apth

  attn <- case (BinAtt `elem` opts) of
    True -> do
      batt <- B.readFile apth
      return $ binAttNode fname ctype batt
    False -> do
      tatt <- readFile apth
      return $ utfAttNode fname ctype tatt

  cdb <- if (NewDoc `elem` opts || UsePost `elem` opts) 
    then do

-- If a new document is desired, create a document with only
-- attachments section, and put it into the database.

      when (Verbose `elem` opts) $ do
        hPutStrLn stderr "Creating a new document..."
      let doc = emptyObj `mergeL` ("_attachments", attn)
          cdbfun = if NewDoc `elem` opts then cDBPutJSON else cDBPostJSON
      cdbfun uri doc >>= return . rsp2CDB

    else do

-- If an existing document is to be updated with an attachment,
-- locate the "_attachment" subnode, and merge the new attachment
-- node into it.

      when (Verbose `elem` opts) $ do
        hPutStrLn stderr "Updating an existing document..."
      doc <- if (hasValueWithName "_attachments") dbjs
        then do atts <- getValueByName "_attachments" dbjs
                attn' <- getValueByName fname attn
                return $ dbjs `mergeL` ("_attachments", atts `mergeL` (fname, attn'))
        else return $ dbjs `mergeL` ("_attachments", attn)
      cDBPutJSON uri doc >>= return . rsp2CDB

  docjs <- case cdb of
    CDBJSON 201 _ _ -> cdb2JSON cdb
    CDBJSON c _ _ -> error $ infoerr c uri
    CDBHTTP c r -> error $ infoerr c uri
    CDBBODY c r -> error $ infoerr c uri
  when (Verbose `elem` opts) $ do
    hPutStrLn stderr "Response:"
    hPutStrLn stderr $ showPP cdb
  when (PrintRev `elem` opts) $ printRev docjs

  exitWith (ExitSuccess)

-- Print DocID and revision on standard output. The output has structure
-- "== docid rev" where two equal signs at the start of the line may be used
-- for grepping.

printRev docjs = do
  rev <- getDocRevision docjs
  docid <- getDocID docjs
  putStrLn $ "== " ++ docid ++ " " ++ rev


