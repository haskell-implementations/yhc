-----------------------------------------------------------------------------
-- |
-- Module      :  Data.JsonNode
-- Copyright   :  (c) Dmitry Golubovsky, 2008       
-- License     :  BSD-style
-- 
-- Maintainer  :  Dmitry Golubovsky <golubovsky@gmail.com>
-- Stability   :  stable
-- Portability :  non-portable (needs Yhc Javascript backend)
--
-- Haskell Interface to CouchDB JSON module
--
-------------------------------------------------------------------------------

module Data.JsonNode (
-- * Data definitions
  JsonNode
 ,JSONPRED
-- * Parsing
 ,parseJSON
 ,parseJSONErr
-- * Interface with JSON nodes: a subset of the GHC-oriented module @Database.CouchDB@.
 ,withNewJSON
 ,constrName
 ,hasValueWithName
 ,getValueByName
 ,getColumn
 ,getArray
 ,getString
 ,getBool
 ,getDouble
-- * Setters: update a JSON node in place.
 ,setString
-- * Build a JSON String out of a JsonNode
 ,toJSONString
) where

import UnsafeJS
import CPS
import Control.Monad

-- |Opaque type for a JSON node.

data JsonNode = JsonNode

instance Show JsonNode where
  show a = unsafeJS "return expEval(a).toSource();"

-- |Parse a 'String' into a 'JsonNode'. If there is an error inside the string,
-- or it contains an \"error\" property, monadic failure will occur.

parseJSON :: (Monad m) => String -> m JsonNode

parseJSON s = case parse' s of
                j | unsafeCheckProperty "error" j id -> unsafeGetProperty "error" j fail
                  | otherwise -> return j

-- |Parse a 'String' into a 'JsonNode'. Monadic failure never occurs (use this
-- function if detailed analysis of error object is necessary).

parseJSONErr :: (Monad m) => String -> m JsonNode

parseJSONErr s = let p = parse' s in p `seq` return p

parse' a = unsafeJS 
  "try {return exprEval(a).toString().parseJSON(null);}catch(e){return {error:e.toString()};}"

type JSONPRED = JsonNode -> Bool

-- |Create an empty JSON node.

withNewJSON :: CPS x JsonNode

withNewJSON c = c $! (wnj 0) where
  wnj a = unsafeJS "return { };"

-- |Retrieve object's constructor name: useful when fetching a value of certain type.

constrName :: (Monad m) => JsonNode -> m String

constrName j | unsafeCheckProperty "constructor" j id = gc 
             | otherwise = fail "no constructor"
  where gc = let c = unsafeGetProperty "constructor" j id
                 n | unsafeCheckProperty "name" c id = unsafeGetProperty "name" c return
             in  n
               

-- |Return True if a value with given name exists within the given JSON object.
-- no recursion is performed into deeper levels.

hasValueWithName :: String     -- ^name of the value to check for
                 -> JSONPRED   -- ^JSON object where the value is looked up
                  
hasValueWithName s j = unsafeCheckProperty s j id

-- |Retrieve a value with given name from the given JSON object: no recursion
-- is performed into deeper levels.

getValueByName :: (Monad m)  -- if found, return value otherwise fail with name
               => String     -- ^name of the value to retrieve
               -> JsonNode   -- ^JSON object where the value is looked up
               -> m JsonNode -- ^returned monadic value


getValueByName s j | unsafeCheckProperty s j id = unsafeGetProperty s j return
                   | otherwise = fail s
    

genericGet :: (Monad m) => String -> (JsonNode -> CPS (m t) t) -> JsonNode -> m t

genericGet s r j = do c <- constrName j
                      case c of
                        s -> r j return
                        _ -> fail $ "not a " ++ s

-- Get a list (list head actually) from a JSON String or Array node.
-- Due to automatic conversion, Javascript Strings and Arrays are 
-- almost indistinguishable in WHNF (the distinction may be made
-- by looking at the first (head) argument of the HSCons data object
-- to see if it is a character). If the list is empty, no distinction
-- is possible at all.

genericGetList :: (Monad m)     -- if a String or Array, return value otherwise fail
               => JsonNode      -- ^JSON object where the value is retrieved from
               -> m ([a], Bool) -- ^retrieved list, non-type-specific

genericGetList j = do c <- constrName j
                      case c of
                        "HSCons" -> unsafeToSelf j $ \l@(h:_) ->
                                      let ch = unsafeCheckProperty "_chr" h id
                                      in  return (l, ch)
                        "HSEOL"  -> return ([], True)
                        _        -> fail $ "not a String nor an Array"


-- |Get a 'String' from a JSON String node, otherwise fail.

getString :: (Monad m) -- if a 'String', return value otherwise fail
          => JsonNode  -- ^JSON object where the value is retrieved from
          -> m String  -- ^retrieved string

getString j = do (l, ch) <- genericGetList j
                 if ch then return l else fail "not a String"
                

-- |Get an Array (['JsonNode']) from a JSON Array node, otherwise fail.

getArray  :: (Monad m)    -- if an Array, return value otherwise fail
          => JsonNode     -- ^JSON object where the value is retrieved from
          -> m [JsonNode] -- ^retrieved string

getArray  j = do (l, ch) <- genericGetList j
                 if (not ch) then return l else fail "not an Array"
                
-- |Given a JSON Array object, retrieve JSON values with the same name
-- from every its member that has it.

getColumn :: (Monad m)
          => String       -- ^column name
          -> JsonNode     -- ^JSON object with array the column will be retrieved from
          -> m [JsonNode] -- ^column retrieved

getColumn s j = getArray j >>= mapM (getValueByName s)
                              

-- |Get a 'Double' from a JSON Number node, otherwise fail.

getDouble :: (Monad m) -- if a 'Double', return value otherwise fail
          => JsonNode  -- ^JSON object where the value is retrieved from
          -> m Double  -- ^retrieved number

getDouble = genericGet "Number" unsafeToNum

-- |Get a 'Boolean' from a JSON Bool node, otherwise fail.

getBool   :: (Monad m) -- if a 'Bool', return value otherwise fail
          => JsonNode  -- ^JSON object where the value is retrieved from
          -> m Bool    -- ^retrieved boolean

getBool   = genericGet "Boolean" unsafeToSelf

-- Setters are like unsafeSetProperty, but more type-aware.

-- |Set or replace a String attribute of a JSON node.

setString :: (Monad m)   -- always returns
          => String      -- ^attribute name
          -> String      -- ^attribute value
          -> JsonNode    -- ^JSON object to modify
          -> m JsonNode  -- ^the same JSON object updated in place

setString n v j = return $ set' n v j where
  set' a b c = unsafeJS "exprEval(c)[exprEval(a)] = exprEval(b) + ''; return c;"

-- |Given a JSON node, create a string to be sent over to the CouchDB server.
-- All keys will be used, no whitelisting is available.

toJSONString :: (Monad m) -- always returns
             => JsonNode  -- ^complete JSON node
             -> m String  -- ^supposedly well-formed JSON string

toJSONString j = return (tjs j) where
  tjs a = unsafeJS "return exprEval(a).toJSONString();"

