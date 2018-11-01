--
-- Created: 9 Jul 2007 by tobbe@tornkvist.org
-- Desc: Transform the parser output into the form that Epretty.hs expects.
--

module Ecore where

import Data.Char
import Ebif
import Bkeep

type EAtom = String                       -- Atom

type EVars = [String]                     -- List of variable names

type Fname = (String,Arity)               -- Function name

data EFname = EFname Fname                -- Function name
    deriving (Show)

data EMod                                 -- Module definition
    = EMod EAtom [Fname] [EFdef]
    deriving (Show)

data EFdef                                -- Named Function definition
    = EFdef EFname EFun
    deriving (Show)

data EFun                                 -- Local Function definition
    = EFun EVars EXpr                     -- FIXME should it be [EXpr] ?
    deriving (Show)

data EClause                              -- Clause definition
    = EClause EPats [EXpr] [EXpr]
    deriving (Show)

type EClauses = [EClause]                 -- List of clauses

type EPats = [EPat]                       -- List of patterns

data EXpr                                 -- Expression
    = EXnil
    | EXvar String
    | EXnum Integer
    | EXfloat Double
    | EXpat EPat
    | EXatom EAtom
    | EXcons EXpr EXpr
    | EXtuple [EXpr]
    | EXfun EFun
    | EXfname Fname
    | EXfatbar String
    | EXfatbar2 String
    | EXcase EVars EClauses
    | EXlet EVars [EXpr] EXpr
    | EXletRec EVars [EXpr] EXpr
    | EXapply EXpr [EXpr]
    | EXprimop EAtom [EXpr]
    | EXcall EXpr EXpr [EXpr]
    | EXunknown String                    -- raw dump of unknown Yhc Core
    | EXthunk EXpr Arity [EXpr]           -- thunk contains expression, arity, and arguments
    | EXcaf String String                 -- a nullary function
    | EXforce EXpr                        -- force the following expression
    deriving (Show)

-- Originally from Parser.hs

data EPat                                 -- Pattern definition
    = EPnil
    | EPvar String
    | EPatom String
    | EPdcare 
    | EPnum Integer
    | EPtuple [EPat]
    | EPtcon String [EPat]
    | EPcons EPat EPat 
    deriving (Show)

-- Build an Erlang list out of a Haskell list.
    
erlList :: [EXpr] -> EXpr
    
erlList = el where
  el [] = EXnil
  el (e:es) = EXcons e (erlList es)



