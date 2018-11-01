module TAbstractEval2
  (gaeEval,gaeEvalConst,gaeEvalExact,gaeSubst,gaeMkMeet) where

import qualified Prelude 
import qualified Hat as T 
import qualified TPreludeBasic 
import TPrelude 
import TBaseDefs 
import TUtils 
import TMyUtils 
import TAbstractVals2 
import TApply 

gaeEval :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun (HExpr Naam) (HExpr Naam))

gaeEval paeEval p =
  T.fun1 aaeEval paeEval p haeEval
  where
  
  haeEval (T.R (HVar _) _) p =
    T.ap1 p18v23 p (gpanic p18v23 p) (T.fromLitString p18v29 p "aeEval(1)")
  haeEval (T.R (HLam _ _) _) p =
    T.ap1 p19v23 p (gpanic p19v23 p) (T.fromLitString p19v29 p "aeEval(2)")
  haeEval (T.R (HTable _) _) p =
    T.ap1 p20v23 p (gpanic p20v23 p) (T.fromLitString p20v29 p "aeEval(3)")
  haeEval (fh@(T.R (HPoint _) _)) p = T.projection p22v23 p fh
  haeEval (T.R (HMeet fes) _) p =
    T.con1 p24v23 p HPoint aHPoint
      (T.ap2 p24v31 p (gfoldr1 p24v31 p) (p24v39 !\/ p)
        (T.ap2 p24v44 p (gmap p24v44 p) (gaeEvalConst p24v48 p) fes))
  haeEval (T.R (HApp (T.R (HTable ft) _) fe2) _) p =
    T.ap1 p27v6 p (gaeEval p27v6 p)
      (T.ap3 p27v14 p (gutSureLookup p27v14 p) ft
        (T.fromLitString p27v29 p "aeEval(5)")
        (T.ap1 p27v42 p (gaeEvalConst p27v42 p) fe2))
  haeEval (T.R (HVAp (T.R (HPoint ff) _) fes) _) p =
    T.con1 p30v6 p HPoint aHPoint
      (T.ap2 p30v14 p (gapApply p30v14 p) ff
        (T.ap2 p30v25 p (gmap p30v25 p) (gaeEvalConst p30v29 p) fes))
  haeEval (T.R (HApp (ff@(T.R (HApp _ _) _)) fsomeArg) _) p =
    T.ap1 p33v6 p (gaeEval p33v6 p)
      (T.con2 p33v14 p HApp aHApp (T.ap1 p33v20 p (gaeEval p33v20 p) ff)
        fsomeArg)
  haeEval (T.R (HApp (ff@(T.R (HPoint _) _)) fe) _) p =
    T.ap1 p36v6 p (gaeEval p36v6 p)
      (T.con2 p36v14 p HVAp aHVAp ff (T.fromExpList p36v21 p [fe]))
  haeEval fx p =
    T.ap1 p38v12 p (gpanic p38v12 p) (T.fromLitString p38v18 p "aeEval(4)")
  

gaeEvalConst :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun (HExpr Naam) Route)

gaeEvalConst paeEvalConst p =
  T.fun1 aaeEvalConst paeEvalConst p haeEvalConst
  where
  
  haeEvalConst fe p =
    T.ccase p46v6 p
      (let
        v46v6v1 (T.R (HPoint fp) _) p = T.projection p46v36 p fp
        v46v6v1 _ p =
          T.ap1 p46v44 p (gpanic p46v44 p)
            (T.fromLitString p46v50 p "aeEvalConst") in (v46v6v1))
      (T.ap1 p46v11 p (gaeEval p46v11 p) fe)
  

gaeEvalExact ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun (HExpr Naam) (T.Fun (T.List (HExpr Naam)) Route))

gaeEvalExact paeEvalExact p =
  T.fun2 aaeEvalExact paeEvalExact p haeEvalExact
  where
  
  haeEvalExact (T.R (HLam fvs fe) _) fargs p =
    T.ccase p54v6 p
      (let
        v54v6v1 (T.R (HPoint fp) _) p = T.projection p55v21 p fp
        v54v6v1 _ p =
          T.ap1 p55v29 p (gpanic p55v29 p)
            (T.fromLitString p55v35 p "aeEvalExact") in (v54v6v1))
      (T.ap1 p54v11 p (gaeEval p54v11 p)
        (T.ap2 p54v19 p (gaeSubst p54v19 p)
          (T.ap2 p54v28 p (gmyZip2 p54v28 p) fvs fargs) fe))
  haeEvalExact _ _ p = T.fatal p
  

gaeSubst ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R (T.Fun (AList Naam (HExpr Naam)) (T.Fun (HExpr Naam) (HExpr Naam)))

gaeSubst paeSubst p =
  T.fun2 aaeSubst paeSubst p haeSubst
  where
  
  haeSubst frho (T.R (HVar fv) _) p =
    T.ap3 p62v29 p (gutSureLookup p62v29 p) frho
      (T.fromLitString p62v46 p "aeSubst") fv
  haeSubst frho (fh@(T.R (HPoint fp) _)) p = T.projection p63v29 p fh
  haeSubst frho (T.R (HLam _ _) _) p =
    T.ap1 p64v29 p (gpanic p64v29 p) (T.fromLitString p64v35 p "aeSubst(1)")
  haeSubst frho (T.R (HMeet fes) _) p =
    T.con1 p65v29 p HMeet aHMeet
      (T.ap2 p65v36 p (gmap p65v36 p) (T.ap1 p65v41 p (gaeSubst p65v41 p) frho)
        fes)
  haeSubst frho (T.R (HTable ft) _) p =
    T.con1 p66v29 p HTable aHTable
      (T.ap2 p66v37 p (gmap2nd p66v37 p)
        (T.ap1 p66v45 p (gaeSubst p66v45 p) frho) ft)
  haeSubst frho (T.R (HApp fe1 fe2) _) p =
    T.con2 p67v29 p HApp aHApp (T.ap2 p67v35 p (gaeSubst p67v35 p) frho fe1)
      (T.ap2 p67v52 p (gaeSubst p67v52 p) frho fe2)
  haeSubst frho (T.R (HVAp ff fes) _) p =
    T.con2 p68v29 p HVAp aHVAp (T.ap2 p68v35 p (gaeSubst p68v35 p) frho ff)
      (T.ap2 p68v51 p (gmap p68v51 p) (T.ap1 p68v56 p (gaeSubst p68v56 p) frho)
        fes)
  haeSubst _ _ p = T.fatal p
  

gaeMkMeet ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R (T.Fun (HExpr Naam) (T.Fun (T.List (HExpr Naam)) (HExpr Naam)))

gaeMkMeet paeMkMeet p =
  T.fun2 aaeMkMeet paeMkMeet p haeMkMeet
  where
  
  haeMkMeet fbottom (T.R T.List _) p = T.projection p75v25 p fbottom
  haeMkMeet fbottom (T.R (T.Cons fx (T.R T.List _)) _) p =
    T.projection p76v25 p fx
  haeMkMeet fbottom fxs p = T.con1 p77v25 p HMeet aHMeet fxs
  

tAbstractEval2 = T.mkModule "AbstractEval2" "AbstractEval2.hs" Prelude.True

aaeEval = T.mkVariable tAbstractEval2 180001 3 1 "aeEval" Prelude.False

aaeEvalConst =
  T.mkVariable tAbstractEval2 450001 3 1 "aeEvalConst" Prelude.False

aaeEvalExact =
  T.mkVariable tAbstractEval2 530001 3 2 "aeEvalExact" Prelude.False

aaeSubst = T.mkVariable tAbstractEval2 620001 3 2 "aeSubst" Prelude.False

aaeMkMeet = T.mkVariable tAbstractEval2 750001 3 2 "aeMkMeet" Prelude.False

p18v1 = T.mkSrcPos tAbstractEval2 180001

p18v23 = T.mkSrcPos tAbstractEval2 180023

p18v29 = T.mkSrcPos tAbstractEval2 180029

p19v23 = T.mkSrcPos tAbstractEval2 190023

p19v29 = T.mkSrcPos tAbstractEval2 190029

p20v23 = T.mkSrcPos tAbstractEval2 200023

p20v29 = T.mkSrcPos tAbstractEval2 200029

p22v23 = T.mkSrcPos tAbstractEval2 220023

p24v23 = T.mkSrcPos tAbstractEval2 240023

p24v31 = T.mkSrcPos tAbstractEval2 240031

p24v39 = T.mkSrcPos tAbstractEval2 240039

p24v44 = T.mkSrcPos tAbstractEval2 240044

p24v48 = T.mkSrcPos tAbstractEval2 240048

p27v6 = T.mkSrcPos tAbstractEval2 270006

p27v14 = T.mkSrcPos tAbstractEval2 270014

p27v29 = T.mkSrcPos tAbstractEval2 270029

p27v42 = T.mkSrcPos tAbstractEval2 270042

p30v6 = T.mkSrcPos tAbstractEval2 300006

p30v14 = T.mkSrcPos tAbstractEval2 300014

p30v25 = T.mkSrcPos tAbstractEval2 300025

p30v29 = T.mkSrcPos tAbstractEval2 300029

p33v6 = T.mkSrcPos tAbstractEval2 330006

p33v14 = T.mkSrcPos tAbstractEval2 330014

p33v20 = T.mkSrcPos tAbstractEval2 330020

p36v6 = T.mkSrcPos tAbstractEval2 360006

p36v14 = T.mkSrcPos tAbstractEval2 360014

p36v21 = T.mkSrcPos tAbstractEval2 360021

p38v12 = T.mkSrcPos tAbstractEval2 380012

p38v18 = T.mkSrcPos tAbstractEval2 380018

p45v1 = T.mkSrcPos tAbstractEval2 450001

p46v6 = T.mkSrcPos tAbstractEval2 460006

p46v11 = T.mkSrcPos tAbstractEval2 460011

p46v36 = T.mkSrcPos tAbstractEval2 460036

p46v44 = T.mkSrcPos tAbstractEval2 460044

p46v50 = T.mkSrcPos tAbstractEval2 460050

p53v1 = T.mkSrcPos tAbstractEval2 530001

p54v6 = T.mkSrcPos tAbstractEval2 540006

p54v11 = T.mkSrcPos tAbstractEval2 540011

p54v19 = T.mkSrcPos tAbstractEval2 540019

p54v28 = T.mkSrcPos tAbstractEval2 540028

p55v21 = T.mkSrcPos tAbstractEval2 550021

p55v29 = T.mkSrcPos tAbstractEval2 550029

p55v35 = T.mkSrcPos tAbstractEval2 550035

p62v1 = T.mkSrcPos tAbstractEval2 620001

p62v29 = T.mkSrcPos tAbstractEval2 620029

p62v46 = T.mkSrcPos tAbstractEval2 620046

p63v29 = T.mkSrcPos tAbstractEval2 630029

p64v29 = T.mkSrcPos tAbstractEval2 640029

p64v35 = T.mkSrcPos tAbstractEval2 640035

p65v29 = T.mkSrcPos tAbstractEval2 650029

p65v36 = T.mkSrcPos tAbstractEval2 650036

p65v41 = T.mkSrcPos tAbstractEval2 650041

p66v29 = T.mkSrcPos tAbstractEval2 660029

p66v37 = T.mkSrcPos tAbstractEval2 660037

p66v45 = T.mkSrcPos tAbstractEval2 660045

p67v29 = T.mkSrcPos tAbstractEval2 670029

p67v35 = T.mkSrcPos tAbstractEval2 670035

p67v52 = T.mkSrcPos tAbstractEval2 670052

p68v29 = T.mkSrcPos tAbstractEval2 680029

p68v35 = T.mkSrcPos tAbstractEval2 680035

p68v51 = T.mkSrcPos tAbstractEval2 680051

p68v56 = T.mkSrcPos tAbstractEval2 680056

p75v1 = T.mkSrcPos tAbstractEval2 750001

p75v25 = T.mkSrcPos tAbstractEval2 750025

p76v25 = T.mkSrcPos tAbstractEval2 760025

p77v25 = T.mkSrcPos tAbstractEval2 770025
