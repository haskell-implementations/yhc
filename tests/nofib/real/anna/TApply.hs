module TApply (gapApply,gapPap_2,gapPap,gapPapConst) where

import qualified Prelude 
import qualified Hat as T 
import qualified TPreludeBasic 
import TPrelude 
import TBaseDefs 
import TUtils 
import TMyUtils 
import TAbstractVals2 

gapApply ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Route (T.Fun (T.List Route) Route))

gapApply papApply p =
  T.fun2 aapApply papApply p hapApply
  where
  
  hapApply (T.R (Rep ffunc) _) fargs p =
    T.ap1 p17v27 p (gapPapConst p17v27 p)
      (T.ap2 p17v39 p (gapPap p17v39 p) ffunc fargs)
  hapApply _ _ p = T.fatal p
  

gapPap_2 ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun Int (T.Fun Frontier (T.Fun (T.List Route) Frontier)))

gapPap_2 papPap_2 p =
  T.fun3 aapPap_2 papPap_2 p hapPap_2
  where
  
  hapPap_2 fargCount (T.R (Min1Max0 far ff1 ff0) _) fargs p =
    let
      gnewf1 pnewf1 p = T.constUse pnewf1 p snewf1
      snewf1 =
        T.constDef p a25v10newf1
          (\ p ->
            T.ap1 p26v15 p (gsort p26v15 p)
              (T.ap1 p26v21 p (gavMinfrel p26v21 p)
                (T.ap1 p0v0 p
                  (T.ap2 p27v20 p (TPrelude.g_foldr p27v20 p)
                    (T.fun2 T.mkLambda p27v20 p
                      (\ f_x f_y p ->
                        T.ccase p0v0 p
                          (let
                            v0v0v1 (T.R (MkFrel ffel) _) p =
                              T.ap1 p27v20 p
                                (T.ap2 p27v20 p (TPrelude.g_filter p27v20 p)
                                  (T.ap3 p29v22 p (gmyAndWith2 p29v22 p)
                                    (p29v34 !<< p)
                                    (T.ap2 p29v39 p (gtake p29v39 p) fargCount
                                      ffel) fargs)
                                  (T.pa1 T.Cons T.cn1 p27v20 p T.aCons
                                    (T.con1 p27v21 p MkFrel aMkFrel
                                      (T.ap2 p27v29 p (gdrop p27v29 p) fargCount
                                        ffel)))) f_y
                            v0v0v1 _ p = T.projection p27v20 p f_y in (v0v0v1))
                          f_x)) ff1) (T.fromExpList p0v0 p []))))
      gnewf0 pnewf0 p = T.constUse pnewf0 p snewf0
      snewf0 =
        T.constDef p a31v10newf0
          (\ p ->
            T.ap1 p32v15 p (gsort p32v15 p)
              (T.ap1 p32v21 p (gavMaxfrel p32v21 p)
                (T.ap1 p0v0 p
                  (T.ap2 p33v20 p (TPrelude.g_foldr p33v20 p)
                    (T.fun2 T.mkLambda p33v20 p
                      (\ f_x f_y p ->
                        T.ccase p0v0 p
                          (let
                            v0v0v1 (T.R (MkFrel ffel) _) p =
                              T.ap1 p33v20 p
                                (T.ap2 p33v20 p (TPrelude.g_filter p33v20 p)
                                  (T.ap3 p35v22 p (gmyAndWith2 p35v22 p)
                                    (p35v34 !<< p) fargs
                                    (T.ap2 p35v44 p (gtake p35v44 p) fargCount
                                      ffel))
                                  (T.pa1 T.Cons T.cn1 p33v20 p T.aCons
                                    (T.con1 p33v21 p MkFrel aMkFrel
                                      (T.ap2 p33v29 p (gdrop p33v29 p) fargCount
                                        ffel)))) f_y
                            v0v0v1 _ p = T.projection p33v20 p f_y in (v0v0v1))
                          f_x)) ff0) (T.fromExpList p0v0 p []))))
      gresult presult p = T.constUse presult p sresult
      sresult =
        T.constDef p a37v10result
          (\ p ->
            T.con3 p37v19 p Min1Max0 aMin1Max0
              (T.ap2 p37v31 p (p37v31 !- p) far fargCount) (gnewf1 p37v42 p)
              (gnewf0 p37v48 p)) in
      (T.cif p39v10 p (T.ap2 p39v22 p (p39v22 !<= p) fargCount far)
        (\ p -> gresult p39v33 p)
        (\ p ->
          T.ap1 p39v45 p (gpanic p39v45 p)
            (T.fromLitString p39v51 p "apPap_2")))
  hapPap_2 _ _ _ p = T.fatal p
  

gapPap :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Rep (T.Fun (T.List Route) Rep))

gapPap papPap p =
  T.fun2 aapPap papPap p hapPap
  where
  
  hapPap (T.R (RepTwo ffr) _) fargs p =
    let
      gargCount pargCount p = T.constUse pargCount p sargCount
      sargCount =
        T.constDef p a47v10argCount
          (\ p -> T.ap1 p47v21 p (glength p47v21 p) fargs) in
      (T.con1 p49v10 p RepTwo aRepTwo
        (T.ap3 p49v18 p (gapPap_2 p49v18 p) (gargCount p49v26 p) ffr fargs))
  hapPap (T.R (Rep1 flf fhfs) _) fargs p =
    let
      gargCount pargCount p = T.constUse pargCount p sargCount
      sargCount =
        T.constDef p a52v10argCount
          (\ p -> T.ap1 p52v21 p (glength p52v21 p) fargs)
      gnew_lf pnew_lf p = T.constUse pnew_lf p snew_lf
      snew_lf =
        T.constDef p a53v10new_lf
          (\ p ->
            T.ap3 p53v19 p (gapPap_2 p53v19 p) (gargCount p53v27 p) flf fargs)
      gnew_hfs pnew_hfs p = T.constUse pnew_hfs p snew_hfs
      snew_hfs =
        T.constDef p a54v10new_hfs
          (\ p ->
            T.ap2 p54v20 p (gmap p54v20 p)
              (T.ap2 p54v25 p (gflip p54v25 p) (gapPap p54v30 p) fargs) fhfs) in
      (T.con2 p56v10 p Rep1 aRep1 (gnew_lf p56v15 p) (gnew_hfs p56v22 p))
  hapPap (T.R (Rep2 flf fmf fhfs) _) fargs p =
    let
      gargCount pargCount p = T.constUse pargCount p sargCount
      sargCount =
        T.constDef p a59v10argCount
          (\ p -> T.ap1 p59v21 p (glength p59v21 p) fargs)
      gnew_lf pnew_lf p = T.constUse pnew_lf p snew_lf
      snew_lf =
        T.constDef p a60v10new_lf
          (\ p ->
            T.ap3 p60v19 p (gapPap_2 p60v19 p) (gargCount p60v27 p) flf fargs)
      gnew_mf pnew_mf p = T.constUse pnew_mf p snew_mf
      snew_mf =
        T.constDef p a61v10new_mf
          (\ p ->
            T.ap3 p61v19 p (gapPap_2 p61v19 p) (gargCount p61v27 p) fmf fargs)
      gnew_hfs pnew_hfs p = T.constUse pnew_hfs p snew_hfs
      snew_hfs =
        T.constDef p a62v10new_hfs
          (\ p ->
            T.ap2 p62v20 p (gmap p62v20 p)
              (T.ap2 p62v25 p (gflip p62v25 p) (gapPap p62v30 p) fargs) fhfs) in
      (T.con3 p64v10 p Rep2 aRep2 (gnew_lf p64v15 p) (gnew_mf p64v22 p)
        (gnew_hfs p64v29 p))
  hapPap _ _ p = T.fatal p
  

gapPapConst :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Rep Route)

gapPapConst papPapConst p =
  T.fun1 aapPapConst papPapConst p hapPapConst
  where
  
  hapPapConst (frep@(T.R (RepTwo (T.R (Min1Max0 far ff1 ff0) _)) _)) p =
    T.cguard p72v9 p
      (T.ap2 p72v9 p (p72v9 !> p) far
        (T.ap1 p72v11 p (TPreludeBasic.gfromInteger p72v11 p)
          (T.conInteger p72v11 p 0))) (\ p -> T.con1 p72v41 p Rep aRep frep)
      (\ p ->
        T.cguard p73v14 p
          (T.ap2 p73v14 p (p73v14 !&& p) (T.ap1 p73v6 p (gnull p73v6 p) ff1)
            (T.ap1 p73v17 p (gnot p73v17 p)
              (T.ap1 p73v22 p (gnull p73v22 p) ff0)))
          (\ p -> T.con0 p73v41 p Zero aZero)
          (\ p ->
            T.cguard p74v20 p
              (T.ap2 p74v20 p (p74v20 !&& p)
                (T.ap1 p74v6 p (gnot p74v6 p)
                  (T.ap1 p74v11 p (gnull p74v11 p) ff1))
                (T.ap1 p74v23 p (gnull p74v23 p) ff0))
              (\ p -> T.con0 p74v41 p One aOne)
              (\ p ->
                T.cguard p75v6 p (gotherwise p75v6 p)
                  (\ p ->
                    T.ap1 p75v41 p (gpanic p75v41 p)
                      (T.fromLitString p75v47 p "apPapConst(1)"))
                  (\ p -> T.fatal p))))
  hapPapConst (frep@(T.R (Rep1 (T.R (Min1Max0 flf_ar flf_f1 flf_f0) _) fhfs) _))
    p =
    T.cguard p78v12 p
      (T.ap2 p78v12 p (p78v12 !> p) flf_ar
        (T.ap1 p78v14 p (TPreludeBasic.gfromInteger p78v14 p)
          (T.conInteger p78v14 p 0))) (\ p -> T.con1 p78v18 p Rep aRep frep)
      (\ p ->
        T.cguard p79v17 p
          (T.ap2 p79v17 p (p79v17 !&& p) (T.ap1 p79v6 p (gnull p79v6 p) flf_f1)
            (T.ap1 p79v20 p (gnot p79v20 p)
              (T.ap1 p79v25 p (gnull p79v25 p) flf_f0)))
          (\ p -> T.con0 p79v41 p Stop1 aStop1)
          (\ p ->
            T.cguard p80v23 p
              (T.ap2 p80v23 p (p80v23 !&& p)
                (T.ap1 p80v6 p (gnot p80v6 p)
                  (T.ap1 p80v11 p (gnull p80v11 p) flf_f1))
                (T.ap1 p80v26 p (gnull p80v26 p) flf_f0))
              (\ p ->
                T.con1 p80v41 p Up1 aUp1
                  (T.ap2 p80v46 p (gmap p80v46 p) (gapPapConst p80v50 p) fhfs))
              (\ p ->
                T.cguard p81v6 p (gotherwise p81v6 p)
                  (\ p ->
                    T.ap1 p81v41 p (gpanic p81v41 p)
                      (T.fromLitString p81v47 p "apPapConst(2)"))
                  (\ p -> T.fatal p))))
  hapPapConst
    (frep@(T.R
        (Rep2 (T.R (Min1Max0 flf_ar flf_f1 flf_f0) _)
          (T.R (Min1Max0 fmf_ar fmf_f1 fmf_f0) _) fhfs) _)) p =
    T.cguard p84v12 p
      (T.ap2 p84v12 p (p84v12 !> p) flf_ar
        (T.ap1 p84v14 p (TPreludeBasic.gfromInteger p84v14 p)
          (T.conInteger p84v14 p 0))) (\ p -> T.con1 p84v18 p Rep aRep frep)
      (\ p ->
        T.cguard p85v17 p
          (T.ap2 p85v17 p (p85v17 !&& p) (T.ap1 p85v6 p (gnull p85v6 p) flf_f1)
            (T.ap1 p85v20 p (gnot p85v20 p)
              (T.ap1 p85v25 p (gnull p85v25 p) flf_f0)))
          (\ p -> T.con0 p85v41 p Stop2 aStop2)
          (\ p ->
            T.cguard p86v17 p
              (T.ap2 p86v17 p (p86v17 !&& p)
                (T.ap1 p86v6 p (gnull p86v6 p) fmf_f1)
                (T.ap1 p86v20 p (gnot p86v20 p)
                  (T.ap1 p86v25 p (gnull p86v25 p) fmf_f0)))
              (\ p -> T.con0 p86v41 p Up2 aUp2)
              (\ p ->
                T.cguard p87v23 p
                  (T.ap2 p87v23 p (p87v23 !&& p)
                    (T.ap1 p87v6 p (gnot p87v6 p)
                      (T.ap1 p87v11 p (gnull p87v11 p) fmf_f1))
                    (T.ap1 p87v26 p (gnull p87v26 p) fmf_f0))
                  (\ p ->
                    T.con1 p87v41 p UpUp2 aUpUp2
                      (T.ap2 p87v48 p (gmap p87v48 p) (gapPapConst p87v52 p)
                        fhfs))
                  (\ p ->
                    T.cguard p88v6 p (gotherwise p88v6 p)
                      (\ p ->
                        T.ap1 p88v41 p (gpanic p88v41 p)
                          (T.fromLitString p88v47 p "apPapConst(3)"))
                      (\ p -> T.fatal p)))))
  hapPapConst _ p = T.fatal p
  

tApply = T.mkModule "Apply" "Apply.hs" Prelude.True

aapApply = T.mkVariable tApply 170001 3 2 "apApply" Prelude.False

aapPap_2 = T.mkVariable tApply 240001 3 3 "apPap_2" Prelude.False

aapPap = T.mkVariable tApply 460001 3 2 "apPap" Prelude.False

aapPapConst = T.mkVariable tApply 710001 3 1 "apPapConst" Prelude.False

a25v10newf1 = T.mkVariable tApply 250010 3 0 "newf1" Prelude.True

a31v10newf0 = T.mkVariable tApply 310010 3 0 "newf0" Prelude.True

a37v10result = T.mkVariable tApply 370010 3 0 "result" Prelude.True

a47v10argCount = T.mkVariable tApply 470010 3 0 "argCount" Prelude.True

a52v10argCount = T.mkVariable tApply 520010 3 0 "argCount" Prelude.True

a53v10new_lf = T.mkVariable tApply 530010 3 0 "new_lf" Prelude.True

a54v10new_hfs = T.mkVariable tApply 540010 3 0 "new_hfs" Prelude.True

a59v10argCount = T.mkVariable tApply 590010 3 0 "argCount" Prelude.True

a60v10new_lf = T.mkVariable tApply 600010 3 0 "new_lf" Prelude.True

a61v10new_mf = T.mkVariable tApply 610010 3 0 "new_mf" Prelude.True

a62v10new_hfs = T.mkVariable tApply 620010 3 0 "new_hfs" Prelude.True

p17v1 = T.mkSrcPos tApply 170001

p17v27 = T.mkSrcPos tApply 170027

p17v39 = T.mkSrcPos tApply 170039

p24v1 = T.mkSrcPos tApply 240001

p25v10 = T.mkSrcPos tApply 250010

p26v15 = T.mkSrcPos tApply 260015

p26v21 = T.mkSrcPos tApply 260021

p0v0 = T.mkSrcPos tApply 0

p27v20 = T.mkSrcPos tApply 270020

p29v22 = T.mkSrcPos tApply 290022

p29v34 = T.mkSrcPos tApply 290034

p29v39 = T.mkSrcPos tApply 290039

p27v21 = T.mkSrcPos tApply 270021

p27v29 = T.mkSrcPos tApply 270029

p31v10 = T.mkSrcPos tApply 310010

p32v15 = T.mkSrcPos tApply 320015

p32v21 = T.mkSrcPos tApply 320021

p33v20 = T.mkSrcPos tApply 330020

p35v22 = T.mkSrcPos tApply 350022

p35v34 = T.mkSrcPos tApply 350034

p35v44 = T.mkSrcPos tApply 350044

p33v21 = T.mkSrcPos tApply 330021

p33v29 = T.mkSrcPos tApply 330029

p37v10 = T.mkSrcPos tApply 370010

p37v19 = T.mkSrcPos tApply 370019

p37v31 = T.mkSrcPos tApply 370031

p37v42 = T.mkSrcPos tApply 370042

p37v48 = T.mkSrcPos tApply 370048

p39v10 = T.mkSrcPos tApply 390010

p39v22 = T.mkSrcPos tApply 390022

p39v33 = T.mkSrcPos tApply 390033

p39v45 = T.mkSrcPos tApply 390045

p39v51 = T.mkSrcPos tApply 390051

p46v1 = T.mkSrcPos tApply 460001

p47v10 = T.mkSrcPos tApply 470010

p47v21 = T.mkSrcPos tApply 470021

p49v10 = T.mkSrcPos tApply 490010

p49v18 = T.mkSrcPos tApply 490018

p49v26 = T.mkSrcPos tApply 490026

p52v10 = T.mkSrcPos tApply 520010

p52v21 = T.mkSrcPos tApply 520021

p53v10 = T.mkSrcPos tApply 530010

p53v19 = T.mkSrcPos tApply 530019

p53v27 = T.mkSrcPos tApply 530027

p54v10 = T.mkSrcPos tApply 540010

p54v20 = T.mkSrcPos tApply 540020

p54v25 = T.mkSrcPos tApply 540025

p54v30 = T.mkSrcPos tApply 540030

p56v10 = T.mkSrcPos tApply 560010

p56v15 = T.mkSrcPos tApply 560015

p56v22 = T.mkSrcPos tApply 560022

p59v10 = T.mkSrcPos tApply 590010

p59v21 = T.mkSrcPos tApply 590021

p60v10 = T.mkSrcPos tApply 600010

p60v19 = T.mkSrcPos tApply 600019

p60v27 = T.mkSrcPos tApply 600027

p61v10 = T.mkSrcPos tApply 610010

p61v19 = T.mkSrcPos tApply 610019

p61v27 = T.mkSrcPos tApply 610027

p62v10 = T.mkSrcPos tApply 620010

p62v20 = T.mkSrcPos tApply 620020

p62v25 = T.mkSrcPos tApply 620025

p62v30 = T.mkSrcPos tApply 620030

p64v10 = T.mkSrcPos tApply 640010

p64v15 = T.mkSrcPos tApply 640015

p64v22 = T.mkSrcPos tApply 640022

p64v29 = T.mkSrcPos tApply 640029

p71v1 = T.mkSrcPos tApply 710001

p72v9 = T.mkSrcPos tApply 720009

p72v11 = T.mkSrcPos tApply 720011

p72v41 = T.mkSrcPos tApply 720041

p73v14 = T.mkSrcPos tApply 730014

p73v6 = T.mkSrcPos tApply 730006

p73v17 = T.mkSrcPos tApply 730017

p73v22 = T.mkSrcPos tApply 730022

p73v41 = T.mkSrcPos tApply 730041

p74v20 = T.mkSrcPos tApply 740020

p74v6 = T.mkSrcPos tApply 740006

p74v11 = T.mkSrcPos tApply 740011

p74v23 = T.mkSrcPos tApply 740023

p74v41 = T.mkSrcPos tApply 740041

p75v6 = T.mkSrcPos tApply 750006

p75v41 = T.mkSrcPos tApply 750041

p75v47 = T.mkSrcPos tApply 750047

p78v12 = T.mkSrcPos tApply 780012

p78v14 = T.mkSrcPos tApply 780014

p78v18 = T.mkSrcPos tApply 780018

p79v17 = T.mkSrcPos tApply 790017

p79v6 = T.mkSrcPos tApply 790006

p79v20 = T.mkSrcPos tApply 790020

p79v25 = T.mkSrcPos tApply 790025

p79v41 = T.mkSrcPos tApply 790041

p80v23 = T.mkSrcPos tApply 800023

p80v6 = T.mkSrcPos tApply 800006

p80v11 = T.mkSrcPos tApply 800011

p80v26 = T.mkSrcPos tApply 800026

p80v41 = T.mkSrcPos tApply 800041

p80v46 = T.mkSrcPos tApply 800046

p80v50 = T.mkSrcPos tApply 800050

p81v6 = T.mkSrcPos tApply 810006

p81v41 = T.mkSrcPos tApply 810041

p81v47 = T.mkSrcPos tApply 810047

p84v12 = T.mkSrcPos tApply 840012

p84v14 = T.mkSrcPos tApply 840014

p84v18 = T.mkSrcPos tApply 840018

p85v17 = T.mkSrcPos tApply 850017

p85v6 = T.mkSrcPos tApply 850006

p85v20 = T.mkSrcPos tApply 850020

p85v25 = T.mkSrcPos tApply 850025

p85v41 = T.mkSrcPos tApply 850041

p86v17 = T.mkSrcPos tApply 860017

p86v6 = T.mkSrcPos tApply 860006

p86v20 = T.mkSrcPos tApply 860020

p86v25 = T.mkSrcPos tApply 860025

p86v41 = T.mkSrcPos tApply 860041

p87v23 = T.mkSrcPos tApply 870023

p87v6 = T.mkSrcPos tApply 870006

p87v11 = T.mkSrcPos tApply 870011

p87v26 = T.mkSrcPos tApply 870026

p87v41 = T.mkSrcPos tApply 870041

p87v48 = T.mkSrcPos tApply 870048

p87v52 = T.mkSrcPos tApply 870052

p88v6 = T.mkSrcPos tApply 880006

p88v41 = T.mkSrcPos tApply 880041

p88v47 = T.mkSrcPos tApply 880047
