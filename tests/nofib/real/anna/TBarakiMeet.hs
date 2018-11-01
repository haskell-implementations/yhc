module TBarakiMeet
  (gbmNorm,gbmNorm_rep,gbmNorm_2,gbmNorm_frel,gbmGLB,gbmGLBrep,gbmGLBfrontier
    ,gbmGLBmax0frontier,gbmMaxAddPtfrel,gbmBelowMax0frel,gbmBelowEQfrel,(!%%)
    ,gbmBelowEQrep,gbmBelowEQfrontier) where

import qualified Prelude 
import qualified Hat as T 
import qualified TPreludeBasic 
import TPrelude 
import TBaseDefs 
import TMyUtils 
import TUtils 
import TAbstractVals2 
import TSuccsAndPreds2 

gbmNorm :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Domain (T.Fun Route Route))

gbmNorm pbmNorm p =
  T.fun2 abmNorm pbmNorm p hbmNorm
  where
  
  hbmNorm (T.R Two _) fr p = T.projection p25v38 p fr
  hbmNorm (T.R (Lift1 fds) _) (fr@(T.R Stop1 _)) p = T.projection p26v38 p fr
  hbmNorm (T.R (Lift1 fds) _) (T.R (Up1 frs) _) p =
    T.con1 p27v38 p Up1 aUp1
      (T.ap3 p27v43 p (gmyZipWith2 p27v43 p) (gbmNorm p27v54 p) fds frs)
  hbmNorm (T.R (Lift2 fds) _) (fr@(T.R Stop2 _)) p = T.projection p28v38 p fr
  hbmNorm (T.R (Lift2 fds) _) (fr@(T.R Up2 _)) p = T.projection p29v38 p fr
  hbmNorm (T.R (Lift2 fds) _) (T.R (UpUp2 frs) _) p =
    T.con1 p30v38 p UpUp2 aUpUp2
      (T.ap3 p30v45 p (gmyZipWith2 p30v45 p) (gbmNorm p30v56 p) fds frs)
  hbmNorm fd (T.R (Rep frep) _) p =
    T.con1 p31v38 p Rep aRep (T.ap2 p31v43 p (gbmNorm_rep p31v43 p) fd frep)
  hbmNorm _ _ p = T.fatal p
  

gbmNorm_rep pbmNorm_rep p =
  T.fun2 abmNorm_rep pbmNorm_rep p hbmNorm_rep
  where
  
  hbmNorm_rep (T.R (Func fdss (T.R Two _)) _) (T.R (RepTwo ffr) _) p =
    T.con1 p35v6 p RepTwo aRepTwo (T.ap2 p35v14 p (gbmNorm_2 p35v14 p) fdss ffr)
  hbmNorm_rep (T.R (Func fdss (T.R (Lift1 fdts) _)) _) (T.R (Rep1 flf fhfs) _)
    p =
    let
      ghf_domains phf_domains p = T.constUse phf_domains p shf_domains
      shf_domains =
        T.constDef p a38v10hf_domains
          (\ p ->
            T.ap2 p38v23 p (gmap p38v23 p)
              (T.ap1 p38v28 p (gavUncurry p38v28 p) fdss) fdts) in
      (T.con2 p40v10 p Rep1 aRep1 (T.ap2 p40v16 p (gbmNorm_2 p40v16 p) fdss flf)
        (T.ap3 p41v16 p (gmyZipWith2 p41v16 p) (gbmNorm_rep p41v27 p)
          (ghf_domains p41v38 p) fhfs))
  hbmNorm_rep (T.R (Func fdss (T.R (Lift2 fdts) _)) _)
    (T.R (Rep2 flf fmf fhfs) _) p =
    let
      ghf_domains phf_domains p = T.constUse phf_domains p shf_domains
      shf_domains =
        T.constDef p a44v10hf_domains
          (\ p ->
            T.ap2 p44v23 p (gmap p44v23 p)
              (T.ap1 p44v28 p (gavUncurry p44v28 p) fdss) fdts) in
      (T.con3 p46v10 p Rep2 aRep2 (T.ap2 p46v16 p (gbmNorm_2 p46v16 p) fdss flf)
        (T.ap2 p46v34 p (gbmNorm_2 p46v34 p) fdss fmf)
        (T.ap3 p47v16 p (gmyZipWith2 p47v16 p) (gbmNorm_rep p47v27 p)
          (ghf_domains p47v38 p) fhfs))
  hbmNorm_rep _ _ p = T.fatal p
  

gbmNorm_2 pbmNorm_2 p =
  T.fun2 abmNorm_2 pbmNorm_2 p hbmNorm_2
  where
  
  hbmNorm_2 fdss (T.R (Min1Max0 far ff1 ff0) _) p =
    let
      gnorm_f0 pnorm_f0 p = T.constUse pnorm_f0 p snorm_f0
      snorm_f0 =
        T.constDef p a51v10norm_f0
          (\ p ->
            T.ap1 p51v20 p (gsort p51v20 p)
              (T.ap2 p51v26 p (gmap p51v26 p)
                (T.ap1 p51v31 p (gbmNorm_frel p51v31 p) fdss) ff0))
      gnorm_f1 pnorm_f1 p = T.constUse pnorm_f1 p snorm_f1
      snorm_f1 =
        T.constDef p a52v10norm_f1
          (\ p -> T.ap2 p52v20 p (gspMin1FromMax0 p52v20 p) fdss ff0) in
      (T.con3 p54v10 p Min1Max0 aMin1Max0 far (gnorm_f1 p54v22 p)
        (gnorm_f0 p54v30 p))
  hbmNorm_2 _ _ p = T.fatal p
  

gbmNorm_frel pbmNorm_frel p =
  T.fun2 abmNorm_frel pbmNorm_frel p hbmNorm_frel
  where
  
  hbmNorm_frel fdss (T.R (MkFrel ffels) _) p =
    T.con1 p57v6 p MkFrel aMkFrel
      (T.ap3 p57v14 p (gmyZipWith2 p57v14 p) (gbmNorm p57v25 p) fdss ffels)
  hbmNorm_frel _ _ p = T.fatal p
  

gbmGLB :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Route (T.Fun Route Route))

gbmGLB pbmGLB p =
  T.fun2 abmGLB pbmGLB p hbmGLB
  where
  
  hbmGLB (T.R (Rep frep1) _) (T.R (Rep frep2) _) p =
    T.con1 p64v31 p Rep aRep (T.ap2 p64v36 p (gbmGLBrep p64v36 p) frep1 frep2)
  hbmGLB _ _ p = T.fatal p
  

gbmGLBrep :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Rep (T.Fun Rep Rep))

gbmGLBrep pbmGLBrep p =
  T.fun2 abmGLBrep pbmGLBrep p hbmGLBrep
  where
  
  hbmGLBrep (T.R (RepTwo ffr1) _) (T.R (RepTwo ffr2) _) p =
    T.con1 p72v6 p RepTwo aRepTwo
      (T.ap2 p72v14 p (gbmGLBfrontier p72v14 p) ffr1 ffr2)
  hbmGLBrep (T.R (Rep1 flf1 fhfs1) _) (T.R (Rep1 flf2 fhfs2) _) p =
    T.con2 p74v6 p Rep1 aRep1
      (T.ap2 p74v12 p (gbmGLBfrontier p74v12 p) flf1 flf2)
      (T.ap3 p74v36 p (gmyZipWith2 p74v36 p) (gbmGLBrep p74v47 p) fhfs1 fhfs2)
  hbmGLBrep (T.R (Rep2 flf1 fmf1 fhfs1) _) (T.R (Rep2 flf2 fmf2 fhfs2) _) p =
    T.con3 p76v6 p Rep2 aRep2
      (T.ap2 p76v12 p (gbmGLBfrontier p76v12 p) flf1 flf2)
      (T.ap2 p76v36 p (gbmGLBfrontier p76v36 p) fmf1 fmf2)
      (T.ap3 p77v12 p (gmyZipWith2 p77v12 p) (gbmGLBrep p77v23 p) fhfs1 fhfs2)
  hbmGLBrep _ _ p = T.fatal p
  

gbmGLBfrontier ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Frontier (T.Fun Frontier Frontier))

gbmGLBfrontier pbmGLBfrontier p =
  T.fun2 abmGLBfrontier pbmGLBfrontier p hbmGLBfrontier
  where
  
  hbmGLBfrontier (T.R (Min1Max0 far1 _ ff0a) _) (T.R (Min1Max0 far2 _ ff0b) _)
    p =
    T.con3 p88v6 p Min1Max0 aMin1Max0 far1 (T.con0 p88v19 p T.List T.aList)
      (T.ap2 p88v23 p (gbmGLBmax0frontier p88v23 p) ff0a ff0b)
  hbmGLBfrontier _ _ p = T.fatal p
  

gbmGLBmax0frontier ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.List FrontierElem)
          (T.Fun (T.List FrontierElem) (T.List FrontierElem)))

gbmGLBmax0frontier pbmGLBmax0frontier p =
  T.fun2 abmGLBmax0frontier pbmGLBmax0frontier p hbmGLBmax0frontier
  where
  
  hbmGLBmax0frontier ff0a ff0b p =
    T.ap3 p96v16 p (gfoldr p96v16 p) (gbmMaxAddPtfrel p96v22 p) ff0a ff0b
  

gbmMaxAddPtfrel pbmMaxAddPtfrel p =
  T.fun2 abmMaxAddPtfrel pbmMaxAddPtfrel p hbmMaxAddPtfrel
  where
  
  hbmMaxAddPtfrel fx fys p =
    T.cguard p99v9 p (T.ap2 p99v9 p (gbmBelowMax0frel p99v9 p) fx fys)
      (\ p -> T.projection p99v31 p fys)
      (\ p ->
        T.cguard p100v6 p (gotherwise p100v6 p)
          (\ p ->
            T.con2 p100v19 p T.Cons T.aCons fx
              (T.ap1 p0v0 p
                (T.ap2 p100v20 p (TPrelude.g_foldr p100v20 p)
                  (T.fun2 T.mkLambda p100v20 p
                    (\ f_x f_y p ->
                      T.ccase p0v0 p
                        (let
                          v0v0v1 fy p =
                            T.ap1 p100v20 p
                              (T.ap2 p100v20 p (TPrelude.g_filter p100v20 p)
                                (T.ap1 p100v34 p (gnot p100v34 p)
                                  (T.ap2 p100v42 p (gbmBelowEQfrel p100v42 p) fy
                                    fx))
                                (T.pa1 T.Cons T.cn1 p100v20 p T.aCons fy)) f_y
                          v0v0v1 _ p = T.projection p100v20 p f_y in (v0v0v1))
                        f_x)) fys) (T.fromExpList p0v0 p [])))
          (\ p -> T.fatal p))
  

gbmBelowMax0frel pbmBelowMax0frel p =
  T.fun2 abmBelowMax0frel pbmBelowMax0frel p hbmBelowMax0frel
  where
  
  hbmBelowMax0frel fpt ff p =
    T.ap2 p102v26 p (gmyAny p102v26 p)
      (T.ap1 p102v37 p (gbmBelowEQfrel p102v37 p) fpt) ff
  

gbmBelowEQfrel ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun FrontierElem (T.Fun FrontierElem Bool))

gbmBelowEQfrel pbmBelowEQfrel p =
  T.fun2 abmBelowEQfrel pbmBelowEQfrel p hbmBelowEQfrel
  where
  
  hbmBelowEQfrel (T.R (MkFrel frs1) _) (T.R (MkFrel frs2) _) p =
    T.ap3 p110v6 p (gmyAndWith2 p110v6 p) (p110v18 !%% p) frs1 frs2
  hbmBelowEQfrel _ _ p = T.fatal p
  

(!%%) :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Route (T.Fun Route Bool))

(!%%) (%%%) p =
  T.fun2 (+%%) (%%%) p (*%%)
  where
  
  (*%%) (T.R Zero _) _ p = T.con0 p117v33 p True aTrue
  (*%%) (T.R One _) (T.R One _) p = T.con0 p118v33 p True aTrue
  (*%%) (T.R One _) (T.R Zero _) p = T.con0 p119v33 p False aFalse
  (*%%) (T.R Stop1 _) _ p = T.con0 p121v33 p True aTrue
  (*%%) (T.R (Up1 frs1) _) (T.R (Up1 frs2) _) p =
    T.ap3 p122v33 p (gmyAndWith2 p122v33 p) (p122v45 !%% p) frs1 frs2
  (*%%) (T.R (Up1 frs1) _) _ p = T.con0 p123v33 p False aFalse
  (*%%) (T.R Stop2 _) _ p = T.con0 p125v33 p True aTrue
  (*%%) (T.R Up2 _) (T.R Stop2 _) p = T.con0 p126v33 p False aFalse
  (*%%) (T.R Up2 _) _ p = T.con0 p127v33 p True aTrue
  (*%%) (T.R (UpUp2 frs1) _) (T.R (UpUp2 frs2) _) p =
    T.ap3 p128v33 p (gmyAndWith2 p128v33 p) (p128v45 !%% p) frs1 frs2
  (*%%) (T.R (UpUp2 frs1) _) _ p = T.con0 p129v33 p False aFalse
  (*%%) (T.R (Rep frep1) _) (T.R (Rep frep2) _) p =
    T.ap2 p131v33 p (gbmBelowEQrep p131v33 p) frep1 frep2
  (*%%) _ _ p = T.fatal p
  

gbmBelowEQrep :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Rep (T.Fun Rep Bool))

gbmBelowEQrep pbmBelowEQrep p =
  T.fun2 abmBelowEQrep pbmBelowEQrep p hbmBelowEQrep
  where
  
  hbmBelowEQrep (T.R (RepTwo ffr1) _) (T.R (RepTwo ffr2) _) p =
    T.ap2 p139v6 p (gbmBelowEQfrontier p139v6 p) ffr1 ffr2
  hbmBelowEQrep (T.R (Rep1 flf1 fhfs1) _) (T.R (Rep1 flf2 fhfs2) _) p =
    T.ap2 p142v32 p (p142v32 !&& p)
      (T.ap2 p142v6 p (gbmBelowEQfrontier p142v6 p) flf1 flf2)
      (T.ap3 p143v6 p (gmyAndWith2 p143v6 p) (gbmBelowEQrep p143v17 p) fhfs1
        fhfs2)
  hbmBelowEQrep (T.R (Rep2 flf1 fmf1 fhfs1) _) (T.R (Rep2 flf2 fmf2 fhfs2) _)
    p =
    T.ap2 p146v32 p (p146v32 !&& p)
      (T.ap2 p146v6 p (gbmBelowEQfrontier p146v6 p) flf1 flf2)
      (T.ap2 p147v32 p (p147v32 !&& p)
        (T.ap2 p147v6 p (gbmBelowEQfrontier p147v6 p) fmf1 fmf2)
        (T.ap3 p148v6 p (gmyAndWith2 p148v6 p) (gbmBelowEQrep p148v17 p) fhfs1
          fhfs2))
  hbmBelowEQrep _ _ p = T.fatal p
  

gbmBelowEQfrontier ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Frontier (T.Fun Frontier Bool))

gbmBelowEQfrontier pbmBelowEQfrontier p =
  T.fun2 abmBelowEQfrontier pbmBelowEQfrontier p hbmBelowEQfrontier
  where
  
  hbmBelowEQfrontier (T.R (Min1Max0 far1 _ ff0a) _)
    (T.R (Min1Max0 far2 _ ff0b) _) p =
    let
      gouter pouter p =
        T.fun1 a162v10outer pouter p houter
        where
        
        houter (T.R T.List _) p = T.con0 p162v28 p True aTrue
        houter (T.R (T.Cons fx fxs) _) p =
          T.cif p163v28 p (T.ap2 p163v31 p (ginner p163v31 p) fx ff0a)
            (\ p -> T.ap1 p163v48 p (gouter p163v48 p) fxs)
            (\ p -> T.con0 p163v62 p False aFalse)
        houter _ p = T.fatal p
        
      ginner pinner p =
        T.fun2 a164v10inner pinner p hinner
        where
        
        hinner fy (T.R T.List _) p = T.con0 p164v28 p False aFalse
        hinner fy (T.R (T.Cons fz fzs) _) p =
          T.cif p165v28 p (T.ap2 p165v34 p (gbmBelowEQfrel p165v34 p) fy fz)
            (\ p -> T.con0 p165v56 p True aTrue)
            (\ p -> T.ap2 p165v66 p (ginner p165v66 p) fy fzs)
        hinner _ _ p = T.fatal p
         in (T.ap1 p167v10 p (gouter p167v10 p) ff0b)
  hbmBelowEQfrontier _ _ p = T.fatal p
  

tBarakiMeet = T.mkModule "BarakiMeet" "BarakiMeet.hs" Prelude.True

abmNorm = T.mkVariable tBarakiMeet 250001 3 2 "bmNorm" Prelude.False

abmNorm_rep = T.mkVariable tBarakiMeet 340001 3 2 "bmNorm_rep" Prelude.False

abmNorm_2 = T.mkVariable tBarakiMeet 500001 3 2 "bmNorm_2" Prelude.False

abmNorm_frel = T.mkVariable tBarakiMeet 560001 3 2 "bmNorm_frel" Prelude.False

abmGLB = T.mkVariable tBarakiMeet 640001 3 2 "bmGLB" Prelude.False

abmGLBrep = T.mkVariable tBarakiMeet 710001 3 2 "bmGLBrep" Prelude.False

abmGLBfrontier =
  T.mkVariable tBarakiMeet 840001 3 2 "bmGLBfrontier" Prelude.False

abmGLBmax0frontier =
  T.mkVariable tBarakiMeet 950001 3 2 "bmGLBmax0frontier" Prelude.False

abmMaxAddPtfrel =
  T.mkVariable tBarakiMeet 980001 3 2 "bmMaxAddPtfrel" Prelude.False

abmBelowMax0frel =
  T.mkVariable tBarakiMeet 1020005 3 2 "bmBelowMax0frel" Prelude.False

abmBelowEQfrel =
  T.mkVariable tBarakiMeet 1090001 3 2 "bmBelowEQfrel" Prelude.False

(+%%) = T.mkVariable tBarakiMeet 1170014 36 2 "%%" Prelude.False

abmBelowEQrep =
  T.mkVariable tBarakiMeet 1380001 3 2 "bmBelowEQrep" Prelude.False

abmBelowEQfrontier =
  T.mkVariable tBarakiMeet 1550001 3 2 "bmBelowEQfrontier" Prelude.False

a38v10hf_domains = T.mkVariable tBarakiMeet 380010 3 0 "hf_domains" Prelude.True

a44v10hf_domains = T.mkVariable tBarakiMeet 440010 3 0 "hf_domains" Prelude.True

a51v10norm_f0 = T.mkVariable tBarakiMeet 510010 3 0 "norm_f0" Prelude.True

a52v10norm_f1 = T.mkVariable tBarakiMeet 520010 3 0 "norm_f1" Prelude.True

a162v10outer = T.mkVariable tBarakiMeet 1620010 3 1 "outer" Prelude.True

a164v10inner = T.mkVariable tBarakiMeet 1640010 3 2 "inner" Prelude.True

p25v1 = T.mkSrcPos tBarakiMeet 250001

p25v38 = T.mkSrcPos tBarakiMeet 250038

p26v38 = T.mkSrcPos tBarakiMeet 260038

p27v38 = T.mkSrcPos tBarakiMeet 270038

p27v43 = T.mkSrcPos tBarakiMeet 270043

p27v54 = T.mkSrcPos tBarakiMeet 270054

p28v38 = T.mkSrcPos tBarakiMeet 280038

p29v38 = T.mkSrcPos tBarakiMeet 290038

p30v38 = T.mkSrcPos tBarakiMeet 300038

p30v45 = T.mkSrcPos tBarakiMeet 300045

p30v56 = T.mkSrcPos tBarakiMeet 300056

p31v38 = T.mkSrcPos tBarakiMeet 310038

p31v43 = T.mkSrcPos tBarakiMeet 310043

p34v1 = T.mkSrcPos tBarakiMeet 340001

p35v6 = T.mkSrcPos tBarakiMeet 350006

p35v14 = T.mkSrcPos tBarakiMeet 350014

p38v10 = T.mkSrcPos tBarakiMeet 380010

p38v23 = T.mkSrcPos tBarakiMeet 380023

p38v28 = T.mkSrcPos tBarakiMeet 380028

p40v10 = T.mkSrcPos tBarakiMeet 400010

p40v16 = T.mkSrcPos tBarakiMeet 400016

p41v16 = T.mkSrcPos tBarakiMeet 410016

p41v27 = T.mkSrcPos tBarakiMeet 410027

p41v38 = T.mkSrcPos tBarakiMeet 410038

p44v10 = T.mkSrcPos tBarakiMeet 440010

p44v23 = T.mkSrcPos tBarakiMeet 440023

p44v28 = T.mkSrcPos tBarakiMeet 440028

p46v10 = T.mkSrcPos tBarakiMeet 460010

p46v16 = T.mkSrcPos tBarakiMeet 460016

p46v34 = T.mkSrcPos tBarakiMeet 460034

p47v16 = T.mkSrcPos tBarakiMeet 470016

p47v27 = T.mkSrcPos tBarakiMeet 470027

p47v38 = T.mkSrcPos tBarakiMeet 470038

p50v1 = T.mkSrcPos tBarakiMeet 500001

p51v10 = T.mkSrcPos tBarakiMeet 510010

p51v20 = T.mkSrcPos tBarakiMeet 510020

p51v26 = T.mkSrcPos tBarakiMeet 510026

p51v31 = T.mkSrcPos tBarakiMeet 510031

p52v10 = T.mkSrcPos tBarakiMeet 520010

p52v20 = T.mkSrcPos tBarakiMeet 520020

p54v10 = T.mkSrcPos tBarakiMeet 540010

p54v22 = T.mkSrcPos tBarakiMeet 540022

p54v30 = T.mkSrcPos tBarakiMeet 540030

p56v1 = T.mkSrcPos tBarakiMeet 560001

p57v6 = T.mkSrcPos tBarakiMeet 570006

p57v14 = T.mkSrcPos tBarakiMeet 570014

p57v25 = T.mkSrcPos tBarakiMeet 570025

p64v1 = T.mkSrcPos tBarakiMeet 640001

p64v31 = T.mkSrcPos tBarakiMeet 640031

p64v36 = T.mkSrcPos tBarakiMeet 640036

p71v1 = T.mkSrcPos tBarakiMeet 710001

p72v6 = T.mkSrcPos tBarakiMeet 720006

p72v14 = T.mkSrcPos tBarakiMeet 720014

p74v6 = T.mkSrcPos tBarakiMeet 740006

p74v12 = T.mkSrcPos tBarakiMeet 740012

p74v36 = T.mkSrcPos tBarakiMeet 740036

p74v47 = T.mkSrcPos tBarakiMeet 740047

p76v6 = T.mkSrcPos tBarakiMeet 760006

p76v12 = T.mkSrcPos tBarakiMeet 760012

p76v36 = T.mkSrcPos tBarakiMeet 760036

p77v12 = T.mkSrcPos tBarakiMeet 770012

p77v23 = T.mkSrcPos tBarakiMeet 770023

p84v1 = T.mkSrcPos tBarakiMeet 840001

p88v6 = T.mkSrcPos tBarakiMeet 880006

p88v19 = T.mkSrcPos tBarakiMeet 880019

p88v23 = T.mkSrcPos tBarakiMeet 880023

p95v1 = T.mkSrcPos tBarakiMeet 950001

p96v16 = T.mkSrcPos tBarakiMeet 960016

p96v22 = T.mkSrcPos tBarakiMeet 960022

p98v1 = T.mkSrcPos tBarakiMeet 980001

p99v9 = T.mkSrcPos tBarakiMeet 990009

p99v31 = T.mkSrcPos tBarakiMeet 990031

p100v6 = T.mkSrcPos tBarakiMeet 1000006

p100v19 = T.mkSrcPos tBarakiMeet 1000019

p0v0 = T.mkSrcPos tBarakiMeet 0

p100v20 = T.mkSrcPos tBarakiMeet 1000020

p100v34 = T.mkSrcPos tBarakiMeet 1000034

p100v42 = T.mkSrcPos tBarakiMeet 1000042

p102v5 = T.mkSrcPos tBarakiMeet 1020005

p102v26 = T.mkSrcPos tBarakiMeet 1020026

p102v37 = T.mkSrcPos tBarakiMeet 1020037

p109v1 = T.mkSrcPos tBarakiMeet 1090001

p110v6 = T.mkSrcPos tBarakiMeet 1100006

p110v18 = T.mkSrcPos tBarakiMeet 1100018

p117v14 = T.mkSrcPos tBarakiMeet 1170014

p117v33 = T.mkSrcPos tBarakiMeet 1170033

p118v33 = T.mkSrcPos tBarakiMeet 1180033

p119v33 = T.mkSrcPos tBarakiMeet 1190033

p121v33 = T.mkSrcPos tBarakiMeet 1210033

p122v33 = T.mkSrcPos tBarakiMeet 1220033

p122v45 = T.mkSrcPos tBarakiMeet 1220045

p123v33 = T.mkSrcPos tBarakiMeet 1230033

p125v33 = T.mkSrcPos tBarakiMeet 1250033

p126v33 = T.mkSrcPos tBarakiMeet 1260033

p127v33 = T.mkSrcPos tBarakiMeet 1270033

p128v33 = T.mkSrcPos tBarakiMeet 1280033

p128v45 = T.mkSrcPos tBarakiMeet 1280045

p129v33 = T.mkSrcPos tBarakiMeet 1290033

p131v33 = T.mkSrcPos tBarakiMeet 1310033

p138v1 = T.mkSrcPos tBarakiMeet 1380001

p139v6 = T.mkSrcPos tBarakiMeet 1390006

p142v32 = T.mkSrcPos tBarakiMeet 1420032

p142v6 = T.mkSrcPos tBarakiMeet 1420006

p143v6 = T.mkSrcPos tBarakiMeet 1430006

p143v17 = T.mkSrcPos tBarakiMeet 1430017

p146v32 = T.mkSrcPos tBarakiMeet 1460032

p146v6 = T.mkSrcPos tBarakiMeet 1460006

p147v32 = T.mkSrcPos tBarakiMeet 1470032

p147v6 = T.mkSrcPos tBarakiMeet 1470006

p148v6 = T.mkSrcPos tBarakiMeet 1480006

p148v17 = T.mkSrcPos tBarakiMeet 1480017

p155v1 = T.mkSrcPos tBarakiMeet 1550001

p162v10 = T.mkSrcPos tBarakiMeet 1620010

p162v28 = T.mkSrcPos tBarakiMeet 1620028

p163v28 = T.mkSrcPos tBarakiMeet 1630028

p163v31 = T.mkSrcPos tBarakiMeet 1630031

p163v48 = T.mkSrcPos tBarakiMeet 1630048

p163v62 = T.mkSrcPos tBarakiMeet 1630062

p164v10 = T.mkSrcPos tBarakiMeet 1640010

p164v28 = T.mkSrcPos tBarakiMeet 1640028

p165v28 = T.mkSrcPos tBarakiMeet 1650028

p165v34 = T.mkSrcPos tBarakiMeet 1650034

p165v56 = T.mkSrcPos tBarakiMeet 1650056

p165v66 = T.mkSrcPos tBarakiMeet 1650066

p167v10 = T.mkSrcPos tBarakiMeet 1670010
