-- Low-level routines to access the Javascript Unicode Characters Database (JUCD).
-- Webpages using this module should load the script file JUCD.js

module Data.Unicode.Unicode where

import UnsafeJS
import Data.Char

ruleFor :: Char -> r
ruleFor = rfa . ord
rfa a = unsafeJS "return findRule(aC, exprEval(a));"

getCategory :: Char -> String
getCategory c = unsafeGetProperty "c" (ruleFor c) $ \v -> v

uToUpper :: Char -> Char
uToUpper = tou . ord
tou a = unsafeJS "return mkChar(uToUpper(exprEval(a)));"

uToLower :: Char -> Char
uToLower = tol . ord
tol a = unsafeJS "return mkChar(uToLower(exprEval(a)));"

