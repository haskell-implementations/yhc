--------------------------------------------------------------------
-- |
-- Module      :  Database.CouchDB
-- Copyright   :  (c) Dmitry Golubovsky, 2007              
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Haskell interface to CouchDB (via HTTP)        
--

module Database.CouchDB (
-- *Data structures
  cDBPort
 ,CDBResult(..)
-- *URI operations
 ,dbURI
 ,dbListURI
 ,dbDBaseURI
 ,dbListDocsURI
 ,dbListCountDocsURI
 ,dbDocURI
-- *HTTP requests
 ,cDBGet
 ,cDBPutStr
 ,cDBPutJSON
 ,cDBPostJSON
 ,cDBDel
 ,cDBTempView
-- *Conversion of HTTP responses
 ,rsp2CDB
 ,rsp2STR
-- *Predicates and JSON retrieval
 ,JSONPRED
 ,cdb2JSON
 ,isNonEmptyList
 ,hasValueWithName
 ,getValueByName
 ,getDBList
 ,getDocRevision
 ,getDocID
 ,getMany
 ,getColumn
 ,getString
 ,getBool
 ,getDouble
-- *Operations on JSON nodes
 ,emptyObj
 ,mergeL
 ,mergeR
 ,mergeUp
 ,unionL
 ,unionR
 ,utfAttNode
 ,binAttNode
 ,delNode
-- *Debug
 ,showPP
) where

import Network.HTTP
import Network.URI
import Data.Maybe
import Text.JSON
import Text.PrettyPrint.HughesPJ
import qualified Data.Map as M
import qualified Codec.Binary.Base64.String as S64
import qualified Codec.Binary.Base64 as B64
import qualified Codec.Binary.UTF8.String as UTF
import qualified Data.ByteString as B

-- |Default value of CouchDB TCP port

cDBPort :: String
cDBPort = "5984"

-- |Data type to describe possible return codes from
-- the CouchDB server. Possible return type may be HTTP
-- error, JSON node, and response body (as 'String').
-- Response headers are not included: use raw HTTP
-- response to access them.

data CDBResult = 
  CDBHTTP Int String           -- ^is returned when no useful information (code, reason)
 |CDBBODY Int String           -- ^there was some content, but with type 
                               --  other than application/json
 |CDBJSON Int String JsonNode  -- ^code, reason, parsed body: returned when
                               --  content type was application/json
  deriving (Show)

-- |Same as 'show' would do, but JSON portion is pretty-printed.

showPP :: CDBResult -> String

showPP (CDBHTTP c r) = "CDBHTTP " ++ show c ++ " " ++ show r
showPP (CDBBODY c r) = "CDBBODY " ++ show c ++ " " ++ show r
showPP (CDBJSON c r j) = "CDBJSON " ++ show c ++ " " ++ show r ++ "\n" ++ (render $ toDocPP j)
                               
-- |Form a base URI for the whole CouchDB server

dbURI :: String    -- ^host 
      -> String    -- ^port
      -> Maybe URI -- ^parsed URI

dbURI host port =
  let colon | length port == 0 = ""
            | otherwise = ":"
  in  parseAbsoluteURI $ "http://" ++ host ++ colon ++ port ++ "/" 

-- |Form an URI to obtain a list of all databases on the server

dbListURI :: URI       -- ^base URI for the CouchDB server (obtained from 'dbURI')
          -> Maybe URI -- ^parsed URI

dbListURI buri = 
  fromJust (parseRelativeReference "/_all_dbs") `relativeTo` buri

-- |Form an URI to perform any operation on a database such as
-- create, delete, or get database information.

dbDBaseURI :: URI       -- ^base URI for the CouchDB server (obtained from 'dbURI')
           -> String    -- ^name of a database
           -> Maybe URI -- ^parsed URI

dbDBaseURI buri dbname = 
  let rel = parseRelativeReference $ "/" ++ dbname ++ "/"
  in  case rel of
        Nothing -> Nothing
        Just dbu -> dbu `relativeTo` buri

-- |Form a document base URI.

dbDocURI dburi docname =
  let rel = parseRelativeReference $ docname ++ "/"
  in  case rel of
        Nothing -> Nothing
        Just dbu -> dbu `relativeTo` dburi

-- |Form an URI to list all documents of a given database

dbListDocsURI :: URI       -- ^base URI for the database (obtained from 'dbDBaseURI')
              -> Maybe URI -- ^parsed URI

dbListDocsURI dburi =
  fromJust (parseRelativeReference "_all_docs") `relativeTo` dburi

-- |Form an URI to list a number of documents of a given database

dbListCountDocsURI :: Int       -- ^how many documents to list 
                   -> URI       -- ^base URI for the database (obtained from 'dbDBaseURI')
                   -> Maybe URI -- ^parsed URI

dbListCountDocsURI n dburi =
  fromJust (parseRelativeReference $ "_all_docs?count=" ++ show n) `relativeTo` dburi



-- |Perform a GET request over the given database URI. Return whatever comes
-- from the HTTP layer. It is important to insert "Accept: application/json"
-- header in order to be able to receive proper content-type header when
-- response body assumes JSON.

cDBGet :: URI -> IO (Result Response)

cDBGet uri = do
  let rq = Request {
    rqURI = uri
   ,rqMethod = GET
   ,rqHeaders = [Header HdrAccept "application/json, */*"]
   ,rqBody = ""
  }
  simpleHTTP rq

-- |Perform a PUT request over the given database URI. Return whatever comes
-- from the HTTP layer. It is important to insert "Accept: application/json"
-- header in order to be able to receive proper content-type header when
-- response body assumes JSON. Information to be sent is assumed to be a
-- UTF-8 encoded string

cDBPutStr :: URI -> String -> IO (Result Response)

cDBPutStr uri body = do
  let rq = Request {
    rqURI = uri
   ,rqMethod = PUT
   ,rqHeaders = [Header HdrAccept "application/json, */*"
                ,Header HdrContentType "text/plain;charset=utf-8"
                ,Header HdrContentLength (show $ length body)]
   ,rqBody = body
  }
  simpleHTTP rq

-- |Perform a PUT request over the given database URI. Return whatever comes
-- from the HTTP layer. It is important to insert "Accept: application/json"
-- header in order to be able to receive proper content-type header when
-- response body assumes JSON. Information to be sent is assumed to be a
-- JSON object.

cDBPutJSON :: URI -> JsonNode -> IO (Result Response)

cDBPutJSON uri jsn = do
  let body = render (toDocPP jsn)
      rq = Request {
    rqURI = uri
   ,rqMethod = PUT
   ,rqHeaders = [Header HdrAccept "application/json, */*"
                ,Header HdrContentType "application/json"
                ,Header HdrContentLength (show $ length body)]
   ,rqBody = body
  }
  simpleHTTP rq

-- |Perform a POST request sending a piece of Javascript to operate
-- on a temporary view. The first argument should contain a database URI.
-- The second argument, if not Nothing, contains query options.

cDBTempView :: URI -> Maybe URI -> String -> IO (Result Response)

cDBTempView dburi mbreq jvs = do
  let tvuri = fromJust (parseRelativeReference "_temp_view") `relativeTo` dburi
      tvquri | isNothing mbreq = tvuri
             | otherwise = fromJust mbreq `relativeTo` fromJust tvuri
      rq = Request {
    rqURI = fromJust tvquri
   ,rqMethod = POST
   ,rqHeaders = [Header HdrAccept "application/json, */*"
                ,Header HdrContentType "text/javascript"
                ,Header HdrContentLength (show $ length jvs)]
   ,rqBody = jvs
  }
  simpleHTTP rq

-- |Perform a POST request over the given database URI. Return whatever comes
-- from the HTTP layer. It is important to insert "Accept: application/json"
-- header in order to be able to receive proper content-type header when
-- response body assumes JSON. Information to be sent is assumed to be a
-- JSON object.

cDBPostJSON :: URI -> JsonNode -> IO (Result Response)

cDBPostJSON uri jsn = do
  let body = render (toDocPP jsn)
      rq = Request {
    rqURI = uri
   ,rqMethod = POST
   ,rqHeaders = [Header HdrAccept "application/json, */*"
                ,Header HdrContentType "application/json"
                ,Header HdrContentLength (show $ length body)]
   ,rqBody = body
  }
  simpleHTTP rq


-- |Perform a DELETE request over the given database URI. Return whatever comes
-- from the HTTP layer. It is important to insert "Accept: application/json"
-- header in order to be able to receive proper content-type header when
-- response body assumes JSON.

cDBDel :: URI -> String -> IO (Result Response)

cDBDel uri rev = do
  let rvuri | rev == "" = Just uri
            | otherwise = fromJust (parseRelativeReference $ "?rev=" ++ rev) `relativeTo` uri
      rq = Request {
    rqURI = fromJust rvuri
   ,rqMethod = DELETE
   ,rqHeaders = [Header HdrAccept "application/json, */*"]
   ,rqBody = ""
  }
  simpleHTTP rq


-- |Convert a HTTP response to CouchDB result type

rsp2CDB :: Result Response -> CDBResult

rsp2CDB (Left ce) = CDBHTTP 999 $ "Connection: " ++ show ce

rsp2CDB (Right rsp@(Response (a, b, c) reason _ body)) =
  let rspn = a * 100 + b * 10 + c
      ctty = fromMaybe "text/plain" $ findHeader HdrContentType rsp
      r | ctty == "application/json" = CDBJSON rspn reason (parse body)
        | length body > 0 = CDBBODY rspn body
        | otherwise = CDBHTTP rspn reason
  in  r 

-- |Convert a HTTP response to a string (basically retrieve body without JSON parsing)

rsp2STR :: Result Response -> CDBResult

rsp2STR (Left ce) = CDBHTTP 999 $ "Connection: " ++ show ce

rsp2STR (Right rsp@(Response (a, b, c) reason _ body)) =
  let rspn = a * 100 + b * 10 + c
  in  CDBBODY rspn body

type JSONPRED = JsonNode -> Bool

-- |Convert a 'CDBResult' structure to a JSON object if one is contained.
-- Otherwise, monadic failure will be caused.

cdb2JSON :: (Monad m)
         => CDBResult -- ^result of a CouchDB request
         -> m JsonNode

cdb2JSON (CDBJSON _ _ jn) = return jn
cdb2JSON _ = fail "No JSON object was returned"

-- |Return True if the given JSON object is a non-empty list.

isNonEmptyList :: JSONPRED

isNonEmptyList (Array jns) = (length jns) > 0
isNonEmptyList _ = False

-- |Return True if a value with given name exists within the given JSON object.
-- no recursion is performed into deeper levels.

hasValueWithName :: String     -- ^name of the value to check for
                 -> JSONPRED   -- ^JSON object where the value is looked up

hasValueWithName s (Object mp) = M.member s mp

hasValueWithName _ _ = False

-- |Retrieve a value with given name from the given JSON object: no recursion
-- is performed into deeper levels.

getValueByName :: (Monad m)  -- ^if found, return value otherwise fail with name
               => String     -- ^name of the value to retrieve
               -> JsonNode   -- ^JSON object where the value is looked up
               -> m JsonNode -- ^returned monadic value

getValueByName s (Object mp) = M.lookup s mp

getValueByName s _ = fail s
         
-- |Convert a JSON node which is expected to be a list of strings into a list of strings
-- representing database names

getDBList :: Monad m => JsonNode -> m [String]

getDBList = return . (getMany getString)


-- |Retrieve a revision string from the result of document creation/update request.
-- The JSON response must contain a "ok":True and "id": value pair, and a "rev" value
-- which is returned.

getDocRevision :: (Monad m) -- ^if found, return value otherwise fail with error reason
               => JsonNode  -- ^JSON object where the value is retrieved from
               -> m String  -- ^retrieved revision string

getDocRevision = getDocItem "rev"

-- |Retrieve a revision string from the result of document creation/update request.
-- The JSON response must contain a "ok":True and "id" value which is returned.

getDocID       :: (Monad m) -- ^if found, return value otherwise fail with error reason
               => JsonNode  -- ^JSON object where the value is retrieved from
               -> m String  -- ^retrieved revision string

getDocID       = getDocItem "id"


getDocItem s jn =
  if hasValueWithName "ok" jn && hasValueWithName "id" jn && hasValueWithName "rev" jn
    then do ok <- getValueByName "ok" jn
            case ok of
              Bool False -> fail "ok not true"
              _          -> getValueByName s jn >>= getString
    else do err <- getValueByName "error" jn >>= getValueByName "id" >>= getString
            fail err

-- |Apply a getter over a JSON Array returning a list of values

getMany :: (JsonNode -> [a]) -- ^getter function: may be monadic, but will work in a
                             -- list mode
        -> JsonNode          -- ^JSON object with array the getter is applied over
        -> [a]               -- ^list of returned values

getMany f (Array jns) = concat $ mapM f jns

getMany _ _ = []

-- |Given a JSON Array object, retrieve JSON values with the same name
-- from every its member that has it.

getColumn :: (Monad m)
          => String     -- ^column name
          -> JsonNode   -- ^JSON object with array the column will be retrieved from
          -> m [JsonNode] -- ^column retrieved

getColumn str = return . (getMany (getValueByName str))

-- |Get a 'String' from a JSON String node, otherwise fail

getString :: (Monad m) -- ^if a 'String', return value otherwise fail
          => JsonNode  -- ^JSON object where the value is retrieved from
          -> m String  -- ^retrieved string

getString (String s) = return s

getString _ = fail "not a String"

-- |Get a 'Bool' from a JSON Bool node, otherwise fail

getBool   :: (Monad m) -- ^if a 'Bool', return value otherwise fail
          => JsonNode  -- ^JSON object where the value is retrieved from
          -> m Bool    -- ^retrieved boolean

getBool   (Bool b) = return b

getBool   _ = fail "not a Bool"

-- |Get a 'Double' from a JSON Number node, otherwise fail

getDouble :: (Monad m) -- ^if a 'Num', return value otherwise fail
          => JsonNode  -- ^JSON object where the value is retrieved from
          -> m Double  -- ^retrieved numeric value

getDouble (Number n) = return n

getDouble _ = fail "not a Number"

-- |Create an 'Object' with empty map.

emptyObj :: JsonNode

emptyObj = Object M.empty

-- Generic node merge. The first node must be an Object.

merge :: String -> JsonNode -> (String, JsonNode) -> JsonNode

merge side (Object m) (s2, n2) = Object (M.insert s2 n2 m)

merge side _ _ = error $ side ++ " node is not an Object"

-- |Merge a JSON node at the right into one at the left. The left node
-- must be an 'Object'. The right node is given a name. If a member with
-- the same name already exists, it will be replaced with the right node.

mergeL :: JsonNode           -- ^should be an 'Object'
       -> (String, JsonNode) -- ^will be inserted into the left node\'s map
       -> JsonNode           -- ^left node updated

mergeL = merge "Left"

-- |Same as 'mergeL', but arguments are flipped: the 'Object' shoulbe at the right side.

mergeR :: (String, JsonNode) -- ^will be inserted into the right node\'s map
       -> JsonNode           -- ^should be an 'Object'
       -> JsonNode           -- ^right node updated

mergeR = flip (merge "Right")

-- Generic union of two nodes: both must be Objects.

union :: JsonNode -> JsonNode -> JsonNode

union (Object ml) (Object mr) = Object (ml `M.union` mr)

union _ _ = error $ "union: both nodes must be Objects"

-- |Left-biased union of two JSON nodes: keys of the left node are preferred.

unionL :: JsonNode -- ^left node
       -> JsonNode -- ^right node
       -> JsonNode -- ^result

unionL = union

-- |Right-biased union of two JSON nodes: keys of the right node are preferred.

unionR :: JsonNode -- ^left node
       -> JsonNode -- ^right node
       -> JsonNode -- ^result

unionR = flip union

-- |Merge two nodes (of any kind) into an Object

mergeUp :: (String, JsonNode) -- ^first node to merge
        -> (String, JsonNode) -- ^second node to merge
        -> JsonNode           -- ^resulting 'Object'

mergeUp n1 n2 = n1 `mergeR` emptyObj `mergeL` n2

-- |Create an utf-8 (text) attachment node. These nodes have to be merged up into
-- an upper level 'Object' node named \"_attachments\". Attachment body may be
-- obtained for example by reading a file with 'getContents' or 'readFile' when
-- the file is expected to be utf-8 encoded.

utfAttNode :: String    -- ^attachment name
           -> String    -- ^content-type
           -> String    -- ^attachment body (Unicode)
           -> JsonNode  -- ^resulting node

utfAttNode aname atype abody =
  (aname, emptyObj `mergeL` ("type", String "base64")      -- CouchDB stores att's in base64
                   `mergeL` ("content-type", String atype) -- the only HTTP header so far
                   `mergeL` ("data", String 
                                   $ concat                -- multi-line base64 should be
                                   $ lines                 -- glued onto a single line
                                   $ (S64.encode . UTF.encodeString) abody)) 
  `mergeR` emptyObj


-- |Create a binary attachment node. These nodes have to be merged up into
-- an upper level 'Object' node named \"_attachments\". Attachment body may be
-- obtained for example by reading a binary file with 'Data.ByteString.readFile'.
-- No assumption will be made about encoding of the file: it will be copied
-- octet by octet. At the moment it is unclear how to force \"Content-Encoding\"
-- header to be sent when downloading an attachment, so no attempt to compress
-- the attachment is made.

binAttNode :: String       -- ^attachment name
           -> String       -- ^content-type
           -> B.ByteString -- ^attachment body (binary)
           -> JsonNode     -- ^resulting node

binAttNode aname atype abody =
  (aname, emptyObj `mergeL` ("type", String "base64")      -- CouchDB stores att's in base64
                   `mergeL` ("content-type", String atype) -- the only HTTP header so far
                   `mergeL` ("data", String 
                                   $ concat                -- multi-line base64 should be
                                   $ lines                 -- glued onto a single line
                                   $ (B64.encode . B.unpack) abody)) 
  `mergeR` emptyObj


-- |Delete a named node. If no node with such name exists, do nothing. If applied
-- to a node which is not an Object, results in an error.

delNode :: JsonNode  -- ^should be an 'Object'
        -> String    -- ^name of the node to delete
        -> JsonNode  -- ^updated node

delNode (Object m) name = Object (M.delete name m)

delNode _ _ = error "delNode: node is not an object"


