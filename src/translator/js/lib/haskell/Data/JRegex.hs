---------------------------------------------------------------------------
-- |
-- Module      :  Data.JRegex
-- Copyright   :  (c) Dmitry Golubovsky, 2007       
-- License     :  BSD-style
-- 
-- Maintainer  :  Dmitry Golubovsky <golubovsky@gmail.com>
-- Stability   :  stable
-- Portability :  non-portable (needs Yhc Javascript backend)
--
-- Interface to Javascript regular expressions.
--
---------------------------------------------------------------------------


module Data.JRegex (
-- $jregex
    JRegex(..)
  , jrxTest
  ) where

import UnsafeJS

{- $jregex
This module exposes a simple interface to the regular expression machinery
provided by web browsers. Please note that semantics of these regular expressions
may vary from browser to browser and does not necessarily match any of Posix,
or Perl regular expressions.
The 'jrxTest' function allows to match a given 'String' against a regular expression.
This is not a "CPS" style function, so the following is valid code:

> valvers vn = 
>  jrxTest (JRegex "^[0-9]+(\\.[0-9]+)*$") -- regexp
>          ""                              -- regexp flags
>          vn                              -- string to match against a regexp

-}

-- |An opaque type to represent a Javascript regular expression
-- (to be distinguished from mere 'String')

newtype JRegex = JRegex String

-- |Match a given 'String' against a regular expression.

jrxTest :: JRegex -- ^regular expression made with the 'JRegex' condtructor
        -> String -- ^regexp flags. If specified, flags can have any 
                  --  combination of the following values: (see Mozilla Developers Connection)
                  --  /g/ - global match, /i/ - ignore case, /m/ - match over multiple lines.
        -> String -- ^a 'String' to match against the regular expression
        -> Bool   -- ^returned value: 'True' if the 'String' provided matches, 'False' otherwise.

jrxTest (JRegex jr) flg mtch = 
  unsafeJS "return new RegExp(exprEval(a),exprEval(b)).test(exprEval(c));"

