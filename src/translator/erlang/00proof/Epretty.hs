--
-- Created: 9 Jul 2007 by tobbe@tornkvist.org
--

module Epretty where

import Data.Char
import Ebif
import Bkeep
import Ecore

--
-- Generate Ecore code from an EMod data structure.
--
epMod :: EMod -> String
epMod (EMod mod fnames defs)
    = let xs = cimplode ((concatMap mkExported fnames) ++ ["'module_info'/0","'module_info'/1"])
          ds = (concatMap (epFdef (bkNew)) defs) ++ (modInfo mod)
      in
        "module '" ++ mod ++ "' [" ++ xs ++ "] \nattributes []\n\n" ++ ds ++ "\nend\n"

mkExported :: Fname -> [String]
mkExported (fname,arity) = 
    [(mkAtom fname) ++ "/" ++ (show n) | n <- [arity]]

--
-- Generate Ecore code from an EFdef data structure.
--
epFdef :: Bkeep -> EFdef -> String
epFdef bk (EFdef (EFname (name,arity)) efun)
    = let px = bkPix bk
          ef = epFun (bkInc bk) efun
      in "\n" ++ px ++ (mkAtom name) ++ "/" ++ (show arity) ++" =\n" ++ ef

--
-- Generate Ecore code from an EFun data structure.
--
epFun :: Bkeep -> EFun -> String
epFun bk (EFun vs expr)                   -- FIXME should it be exprs ?
    = let px = bkPix bk
          vars = cimplode vs
          xpr = epXpr (bkInc bk) expr
      in px ++ "fun (" ++ vars ++ ") ->\n" ++ xpr

--
-- Generate Ecore code from an EXpr data structure.
--
epXpr :: Bkeep -> EXpr -> String
epXpr bk (EXunknown s) = epXpr bk $ 
  EXcall (EXatom "erlang") (EXatom "error") [(erlList $ map (EXnum . fromIntegral . ord) s)]
epXpr bk EXnil = bkPix bk ++ "[]"
epXpr bk (EXnum n) = bkPix bk ++ (show n)
epXpr bk (EXfloat f) = bkPix bk ++ (show f)
epXpr bk (EXvar v) = bkPix bk ++ v
epXpr bk (EXatom v) = bkPix bk ++ "'" ++ v ++"'"
epXpr bk (EXpat p) = bkPix bk ++ (epPat (bkNew) p)
epXpr bk (EXfname (name,arity)) = bkPix bk ++ (mkAtom name) ++ "/" ++ (show arity)
epXpr bk (EXtuple ts) 
    = let px = bkPix bk
          bk2 = bkPix0 bk
          elems = (cimplode . map (epXpr bk2)) ts
      in px ++ "{" ++ elems ++ "}"
epXpr bk (EXcons e1 e2) 
    = let px = bkPix bk
          bk2 = bkPix0 bk
          p1 = epXpr bk2 e1
          p2 = epXpr bk2 e2
      in px ++ "[" ++ p1 ++ "|" ++ p2 ++ "]"
epXpr bk (EXprimop a xs) 
    = let px = bkPix bk
          bk2 = bkInc bk
          atom = mkAtom a
          args = (cimplode . map (epXpr (bkPix0 bk))) xs
      in px ++ "primop " ++ atom ++ "\n" ++ bkPix bk2 ++ "(" ++ args ++ ")"
epXpr bk (EXapply x xs) 
    = let px = bkPix bk
          bk2 = bkInc bk
          expr = epXpr (bkPix0 bk) x
          args = (cimplode . map (epXpr (bkPix0 bk))) xs
      in px ++ "apply " ++ expr ++  "(" ++ args ++ ")"
epXpr bk (EXcall x1 x2 xs) 
    = let px = bkPix bk
          bk2 = bkInc bk
          expr1 = epXpr (bkPix0 bk) x1
          expr2 = epXpr (bkPix0 bk) x2
          args = (cimplode . map (epXpr (bkPix0 bk))) xs
      in px ++ "call " ++ expr1 ++ ":" ++ expr2 ++ "(" ++ args ++ ")"
epXpr bk (EXcase vs cs) 
    = let px = bkPix bk
          vars = cimplode vs
          cls  = concatMap (epCls (bkInc bk)) cs
      in px ++ "case <" ++ vars ++ "> of\n" ++ cls ++ "\n" ++ px ++ "end\n"
epXpr bk (EXlet vs xs1 xs2) 
    = let px = bkPix bk
          bk' = bkInc bk
          px2 = bkPix bk'
          bk'' = bkInc bk'
          vars = cimplode vs
          ex1 = (rmSP . cimplode . map (epXpr bk'')) xs1
          ex2 = (rmSP . (epXpr bk)) xs2
      in px ++ "let <" ++ vars ++ "> =\n" ++ 
         px2 ++ "<" ++ ex1 ++ ">\n" ++
         px2 ++ "in " ++ ex2 ++ "\n"
epXpr bk (EXfun (EFun vs expr)) 
    = let px = bkPix bk
          vars = cimplode vs
          xpr = epXpr (bkInc bk) expr
      in
        px ++ "fun (" ++ vars ++") ->\n" ++ xpr

epXpr bk (EXforce (EXthunk (EXtuple [EXatom m, EXatom f]) ar args)) 
  | ar == length args = epXpr bk $ EXcall (EXatom m) (EXatom f) args

epXpr bk (EXforce c@(EXcall (EXatom "erlang") (EXatom _) _)) = epXpr bk c

epXpr bk (EXforce (EXcaf m f)) = epXpr bk $
  EXcall (EXatom m) (EXatom f) []

epXpr bk (EXforce e) = epXpr bk $ 
  EXcall (EXatom $ fst erlforce) (EXatom $ snd erlforce) [e]

epXpr bk (EXthunk efn ar args) = epXpr bk $ 
  EXtuple [EXatom "@ap", efn, EXnum $ fromIntegral ar, erlList args]

epXpr bk (EXcaf m f) = epXpr bk $
  EXtuple [EXatom "@caf", EXatom m, EXatom f]

-- Erlang name of the force primitive: hserl:force.

erlforce = ("hserl", "force")

rmSP :: String -> String
rmSP = dropWhile isSpace

--
-- Generate Ecore code from an EClause data structure.
--
epCls :: Bkeep -> EClause -> String
epCls bk (EClause eps x1 x2)
    = let px = bkPix bk
          ps = (cimplode . map (epPat (bkPix0 bk))) eps
          y1 = (cimplode . map (epXpr (bkPix0 bk))) x1
          y2 = (cimplode . map (epXpr (bkInc bk))) x2
      in px ++ "<" ++ ps ++ "> when " ++ y1 ++" ->\n" ++ y2 ++ "\n"

lcase = map toLower

--
-- Generate Ecore code from an EPat data structure.
--
epPat :: Bkeep -> EPat -> String
epPat bk EPnil = bkPix bk ++ "[]"
epPat bk EPdcare = bkPix bk ++ "_"
epPat bk (EPvar v) = bkPix bk ++ v
epPat bk (EPnum n) = bkPix bk ++ (show n)
epPat bk (EPatom v) = bkPix bk ++ "'" ++ v ++ "'"
epPat bk (EPtcon c []) = bkPix bk ++ "'" ++ (lcase c) ++ "'"    -- FIXME lcase... ?
epPat bk (EPtcon c p)
    = let px = bkPix bk
          x = cimplode . (map $ epPat bk) $ p
          tc = "'" ++ (lcase c) ++ "'"                          -- FIXME lower... ?
      in px ++ "{" ++ tc ++ "," ++ x ++ "}"
epPat bk (EPtuple ps)
    = let px = bkPix bk
          xs = cimplode . (map $ epPat bk) $ ps
      in px ++ "{" ++ xs ++ "}"
epPat bk (EPcons p1 p2) 
    = let px = bkPix bk
          x1 = epPat bk p1
          x2 = epPat bk p2
      in px ++ "[" ++ x1 ++ "|" ++ x2 ++ "]"


cimplode = implode ","

cNLimplode = implode ",\n"

--
-- Implode a list of strings using the given separator. 
--
implode :: String -> [String] -> String
implode sep ws = concat $ interleave ws sep
 
interleave :: [String] -> String -> [String]
interleave [] _     = []
interleave [x] _    = [x]
interleave (x:xs) s = x:s:(interleave xs s)

mkAtom :: String -> String
mkAtom a = "'" ++ a ++ "'"



modInfo :: String -> String
modInfo m = "\n\n'module_info'/0 = fun () -> call 'erlang':'get_module_info' ('"
         ++ m 
         ++ "')\n"
         ++ "'module_info'/1 = fun (_cor0) -> call 'erlang':'get_module_info' ('"
         ++ m
         ++ "', _cor0)"



