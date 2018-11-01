module TDomainExpr
  (gdxApplyDSubst_2,gdxApplyDSubst,gdxNormaliseDExpr,gdxContainsFnSpace
    ,gdxContainsSubsidiaryFnSpace,gdxDiff,gdxDiff_aux,gdxDiff_list) where

import qualified Prelude 
import qualified Hat as T 
import qualified TPreludeBasic 
import TPrelude 
import TBaseDefs 
import TUtils 
import TMyUtils 

gdxApplyDSubst_2 :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun DExpr Domain)

gdxApplyDSubst_2 pdxApplyDSubst_2 p =
  T.fun1 adxApplyDSubst_2 pdxApplyDSubst_2 p hdxApplyDSubst_2
  where
  
  hdxApplyDSubst_2 (T.R DXTwo _) p = T.con0 p16v39 p Two aTwo
  hdxApplyDSubst_2 (T.R (DXVar _) _) p = T.con0 p17v39 p Two aTwo
  hdxApplyDSubst_2 (T.R (DXLift1 (T.R T.List _)) _) p = T.con0 p18v39 p Two aTwo
  hdxApplyDSubst_2 (T.R (DXLift1 fdxs) _) p =
    T.con1 p19v39 p Lift1 aLift1
      (T.ap2 p19v46 p (gmap p19v46 p) (gdxApplyDSubst_2 p19v50 p) fdxs)
  hdxApplyDSubst_2 (T.R (DXLift2 (T.R T.List _)) _) p =
    T.con1 p20v39 p Lift1 aLift1
      (T.fromExpList p20v45 p [T.con0 p20v46 p Two aTwo])
  hdxApplyDSubst_2 (T.R (DXLift2 fdxs) _) p =
    T.con1 p23v39 p Lift2 aLift2
      (T.ap2 p23v46 p (gmap p23v46 p) (gdxApplyDSubst_2 p23v50 p) fdxs)
  hdxApplyDSubst_2 (T.R (DXFunc fdxs fdxt) _) p =
    T.con2 p24v39 p Func aFunc
      (T.ap2 p24v45 p (gmap p24v45 p) (gdxApplyDSubst_2 p24v49 p) fdxs)
      (T.ap1 p25v45 p (gdxApplyDSubst_2 p25v45 p) fdxt)
  hdxApplyDSubst_2 _ p = T.fatal p
  

gdxApplyDSubst ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun DSubst (T.Fun DExpr Domain))

gdxApplyDSubst pdxApplyDSubst p =
  T.fun2 adxApplyDSubst pdxApplyDSubst p hdxApplyDSubst
  where
  
  hdxApplyDSubst frho (T.R DXTwo _) p = T.con0 p32v27 p Two aTwo
  hdxApplyDSubst frho (T.R (DXVar falpha) _) p =
    T.ap3 p33v40 p (gutSureLookup p33v40 p) frho
      (T.fromLitString p33v57 p "dxApplySubst") falpha
  hdxApplyDSubst frho (T.R (DXLift1 (T.R T.List _)) _) p =
    T.con0 p34v40 p Two aTwo
  hdxApplyDSubst frho (T.R (DXLift1 fdxs) _) p =
    T.con1 p35v40 p Lift1 aLift1
      (T.ap2 p35v47 p (gmap p35v47 p)
        (T.ap1 p35v52 p (gdxApplyDSubst p35v52 p) frho) fdxs)
  hdxApplyDSubst frho (T.R (DXLift2 (T.R T.List _)) _) p =
    T.con1 p36v40 p Lift1 aLift1
      (T.fromExpList p36v46 p [T.con0 p36v47 p Two aTwo])
  hdxApplyDSubst frho (T.R (DXLift2 fdxs) _) p =
    T.con1 p39v40 p Lift2 aLift2
      (T.ap2 p39v47 p (gmap p39v47 p)
        (T.ap1 p39v52 p (gdxApplyDSubst p39v52 p) frho) fdxs)
  hdxApplyDSubst frho (T.R (DXFunc fdxs fdxt) _) p =
    T.con2 p40v40 p Func aFunc
      (T.ap2 p40v46 p (gmap p40v46 p)
        (T.ap1 p40v51 p (gdxApplyDSubst p40v51 p) frho) fdxs)
      (T.ap2 p41v46 p (gdxApplyDSubst p41v46 p) frho fdxt)
  hdxApplyDSubst _ _ p = T.fatal p
  

gdxNormaliseDExpr :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun DExpr DExpr)

gdxNormaliseDExpr pdxNormaliseDExpr p =
  T.fun1 adxNormaliseDExpr pdxNormaliseDExpr p hdxNormaliseDExpr
  where
  
  hdxNormaliseDExpr (T.R (DXFunc fdss (T.R (DXFunc fdss2 fdt) _)) _) p =
    T.ap1 p49v6 p (gdxNormaliseDExpr p49v6 p)
      (T.con2 p49v24 p DXFunc aDXFunc (T.ap2 p49v35 p (p49v35 !++ p) fdss fdss2)
        fdt)
  hdxNormaliseDExpr (T.R (DXFunc fdss fdt) _) p =
    T.con2 p51v6 p DXFunc aDXFunc
      (T.ap2 p51v14 p (gmap p51v14 p) (gdxNormaliseDExpr p51v18 p) fdss)
      (T.ap1 p51v41 p (gdxNormaliseDExpr p51v41 p) fdt)
  hdxNormaliseDExpr (T.R DXTwo _) p = T.con0 p53v36 p DXTwo aDXTwo
  hdxNormaliseDExpr (T.R (DXLift1 fdxs) _) p =
    T.con1 p54v36 p DXLift1 aDXLift1
      (T.ap2 p54v45 p (gmap p54v45 p) (gdxNormaliseDExpr p54v49 p) fdxs)
  hdxNormaliseDExpr (T.R (DXLift2 fdxs) _) p =
    T.con1 p55v36 p DXLift2 aDXLift2
      (T.ap2 p55v45 p (gmap p55v45 p) (gdxNormaliseDExpr p55v49 p) fdxs)
  hdxNormaliseDExpr (T.R (DXVar fv) _) p = T.con1 p56v36 p DXVar aDXVar fv
  hdxNormaliseDExpr _ p = T.fatal p
  

gdxContainsFnSpace :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun DExpr Bool)

gdxContainsFnSpace pdxContainsFnSpace p =
  T.fun1 adxContainsFnSpace pdxContainsFnSpace p hdxContainsFnSpace
  where
  
  hdxContainsFnSpace (T.R DXTwo _) p = T.con0 p63v37 p False aFalse
  hdxContainsFnSpace (T.R (DXLift1 fdxs) _) p =
    T.ap2 p64v37 p (gmyAny p64v37 p) (gdxContainsFnSpace p64v43 p) fdxs
  hdxContainsFnSpace (T.R (DXLift2 fdxs) _) p =
    T.ap2 p65v37 p (gmyAny p65v37 p) (gdxContainsFnSpace p65v43 p) fdxs
  hdxContainsFnSpace (T.R (DXFunc _ _) _) p = T.con0 p66v37 p True aTrue
  hdxContainsFnSpace (T.R (DXVar _) _) p = T.con0 p67v37 p False aFalse
  hdxContainsFnSpace _ p = T.fatal p
  

gdxContainsSubsidiaryFnSpace ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun DExpr Bool)

gdxContainsSubsidiaryFnSpace pdxContainsSubsidiaryFnSpace p =
  T.fun1 adxContainsSubsidiaryFnSpace pdxContainsSubsidiaryFnSpace p
    hdxContainsSubsidiaryFnSpace
  where
  
  hdxContainsSubsidiaryFnSpace (T.R DXTwo _) p = T.con0 p75v6 p False aFalse
  hdxContainsSubsidiaryFnSpace (T.R (DXLift1 fdxs) _) p =
    T.ap2 p78v6 p (gmyAny p78v6 p) (gdxContainsFnSpace p78v12 p) fdxs
  hdxContainsSubsidiaryFnSpace (T.R (DXLift2 fdxs) _) p =
    T.ap2 p81v6 p (gmyAny p81v6 p) (gdxContainsFnSpace p81v12 p) fdxs
  hdxContainsSubsidiaryFnSpace (T.R (DXFunc fdxss fdxt) _) p =
    T.ap2 p84v35 p (p84v35 !|| p)
      (T.ap2 p84v6 p (gmyAny p84v6 p) (gdxContainsFnSpace p84v12 p) fdxss)
      (T.ap1 p84v38 p (gdxContainsFnSpace p84v38 p) fdxt)
  hdxContainsSubsidiaryFnSpace (T.R (DXVar _) _) p = T.con0 p87v6 p False aFalse
  hdxContainsSubsidiaryFnSpace _ p = T.fatal p
  

gdxDiff ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun Domain (T.Fun Domain (T.Tuple2 DExpr DSubst)))

gdxDiff pdxDiff p =
  T.fun2 adxDiff pdxDiff p hdxDiff
  where
  
  hdxDiff fdb fds p =
    T.ccase p95v6 p
      (let
        v95v6v1 (T.R (T.Tuple2 fdexpr (T.R (T.Tuple2 fnum fdsubst) _)) _) p =
          T.con2 p98v35 p T.Tuple2 T.aTuple2 fdexpr fdsubst
        v95v6v1 _ p = T.fatal p in (v95v6v1))
      (T.ap4 p96v9 p (gdoStatefulOp2 p96v9 p) (gdxDiff_aux p96v23 p)
        (T.con2 p96v34 p T.Tuple2 T.aTuple2
          (T.ap1 p96v35 p (gfromEnum p96v35 p) (T.conChar p96v44 p 'a'))
          (T.con0 p96v49 p T.List T.aList)) fds fdb)
  

gdxDiff_aux pdxDiff_aux p =
  T.fun2 adxDiff_aux pdxDiff_aux p hdxDiff_aux
  where
  
  hdxDiff_aux (T.R Two _) (T.R Two _) p =
    T.ap1 p102v6 p (greturnS p102v6 p) (T.con0 p102v14 p DXTwo aDXTwo)
  hdxDiff_aux (T.R Two _) fnot_two p =
    T.ap2 p105v47 p (gthenS p105v47 p) (gfetchS p105v6 p)
      (T.fun1 T.mkLambda p105v57 p
        (\ v105v57v1 p ->
          case (v105v57v1) of
            (T.R (T.Tuple2 fn fsub) _) ->
              T.ap2 p106v50 p (gthenS p106v50 p)
                (T.ap1 p106v6 p (gassignS p106v6 p)
                  (T.con2 p106v14 p T.Tuple2 T.aTuple2
                    (T.ap2 p106v16 p (p106v16 !+ p) fn
                      (T.ap1 p106v17 p (TPreludeBasic.gfromInteger p106v17 p)
                        (T.conInteger p106v17 p 1)))
                    (T.con2 p106v41 p T.Cons T.aCons
                      (T.con2 p106v20 p T.Tuple2 T.aTuple2
                        (T.fromExpList p106v21 p
                          [T.ap1 p106v22 p (gtoEnum p106v22 p) fn]) fnot_two)
                      fsub)))
                (T.fun1 T.mkLambda p106v60 p
                  (\ v106v60v1 p ->
                    case (v106v60v1) of
                      (T.R T.Tuple0 _) ->
                        T.ap1 p107v6 p (greturnS p107v6 p)
                          (T.con1 p107v15 p DXVar aDXVar
                            (T.fromExpList p107v21 p
                              [T.ap1 p107v22 p (gtoEnum p107v22 p) fn]))
                      _ -> T.fatal p))
            _ -> T.fatal p))
  hdxDiff_aux (T.R (Lift1 fds1) _) (T.R (Lift1 fds2) _) p =
    T.ap2 p111v40 p (gthenS p111v40 p)
      (T.ap2 p111v6 p (gdxDiff_list p111v6 p) fds1 fds2)
      (T.fun1 T.mkLambda p111v49 p
        (\ fnew_ds1_ds2 p ->
          T.ap1 p112v6 p (greturnS p112v6 p)
            (T.con1 p112v15 p DXLift1 aDXLift1 fnew_ds1_ds2)))
  hdxDiff_aux (T.R (Lift2 fds1) _) (T.R (Lift2 fds2) _) p =
    T.ap2 p116v40 p (gthenS p116v40 p)
      (T.ap2 p116v6 p (gdxDiff_list p116v6 p) fds1 fds2)
      (T.fun1 T.mkLambda p116v49 p
        (\ fnew_ds1_ds2 p ->
          T.ap1 p117v6 p (greturnS p117v6 p)
            (T.con1 p117v15 p DXLift2 aDXLift2 fnew_ds1_ds2)))
  hdxDiff_aux (T.R (Func fdss1 fdt1) _) (T.R (Func fdss2 fdt2) _) p =
    T.ap2 p121v40 p (gthenS p121v40 p)
      (T.ap2 p121v6 p (gdxDiff_list p121v6 p) fdss1 fdss2)
      (T.fun1 T.mkLambda p121v49 p
        (\ fnew_dss1_dss2 p ->
          T.ap2 p122v40 p (gthenS p122v40 p)
            (T.ap2 p122v6 p (gdxDiff_aux p122v6 p) fdt1 fdt2)
            (T.fun1 T.mkLambda p122v49 p
              (\ fnew_dt1_dt2 p ->
                T.ap1 p123v6 p (greturnS p123v6 p)
                  (T.con2 p123v15 p DXFunc aDXFunc fnew_dss1_dss2
                    fnew_dt1_dt2)))))
  hdxDiff_aux fother1 fother2 p =
    T.ap1 p127v6 p (gpanic p127v6 p) (T.fromLitString p127v12 p "dxDiff_aux")
  

gdxDiff_list pdxDiff_list p =
  T.fun2 adxDiff_list pdxDiff_list p hdxDiff_list
  where
  
  hdxDiff_list (T.R T.List _) (T.R T.List _) p =
    T.ap1 p131v6 p (greturnS p131v6 p) (T.con0 p131v14 p T.List T.aList)
  hdxDiff_list (T.R (T.Cons fa fas) _) (T.R (T.Cons fb fbs) _) p =
    T.ap2 p134v40 p (gthenS p134v40 p)
      (T.ap2 p134v6 p (gdxDiff_aux p134v6 p) fa fb)
      (T.fun1 T.mkLambda p134v50 p
        (\ fnew_a_b p ->
          T.ap2 p135v40 p (gthenS p135v40 p)
            (T.ap2 p135v6 p (gdxDiff_list p135v6 p) fas fbs)
            (T.fun1 T.mkLambda p135v50 p
              (\ fnew_as_bs p ->
                T.ap1 p136v6 p (greturnS p136v6 p)
                  (T.con2 p136v23 p T.Cons T.aCons fnew_a_b fnew_as_bs)))))
  hdxDiff_list fother1 fother2 p =
    T.ap1 p140v6 p (gpanic p140v6 p)
      (T.fromLitString p140v12 p "dxDiff_list: unequal lists")
  

tDomainExpr = T.mkModule "DomainExpr" "DomainExpr.hs" Prelude.True

adxApplyDSubst_2 =
  T.mkVariable tDomainExpr 160001 3 1 "dxApplyDSubst_2" Prelude.False

adxApplyDSubst =
  T.mkVariable tDomainExpr 320001 3 2 "dxApplyDSubst" Prelude.False

adxNormaliseDExpr =
  T.mkVariable tDomainExpr 480001 3 1 "dxNormaliseDExpr" Prelude.False

adxContainsFnSpace =
  T.mkVariable tDomainExpr 630001 3 1 "dxContainsFnSpace" Prelude.False

adxContainsSubsidiaryFnSpace =
  T.mkVariable tDomainExpr 740001 3 1 "dxContainsSubsidiaryFnSpace"
    Prelude.False

adxDiff = T.mkVariable tDomainExpr 940001 3 2 "dxDiff" Prelude.False

adxDiff_aux = T.mkVariable tDomainExpr 1010001 3 2 "dxDiff_aux" Prelude.False

adxDiff_list = T.mkVariable tDomainExpr 1300001 3 2 "dxDiff_list" Prelude.False

p16v1 = T.mkSrcPos tDomainExpr 160001

p16v39 = T.mkSrcPos tDomainExpr 160039

p17v39 = T.mkSrcPos tDomainExpr 170039

p18v39 = T.mkSrcPos tDomainExpr 180039

p19v39 = T.mkSrcPos tDomainExpr 190039

p19v46 = T.mkSrcPos tDomainExpr 190046

p19v50 = T.mkSrcPos tDomainExpr 190050

p20v39 = T.mkSrcPos tDomainExpr 200039

p20v45 = T.mkSrcPos tDomainExpr 200045

p20v46 = T.mkSrcPos tDomainExpr 200046

p23v39 = T.mkSrcPos tDomainExpr 230039

p23v46 = T.mkSrcPos tDomainExpr 230046

p23v50 = T.mkSrcPos tDomainExpr 230050

p24v39 = T.mkSrcPos tDomainExpr 240039

p24v45 = T.mkSrcPos tDomainExpr 240045

p24v49 = T.mkSrcPos tDomainExpr 240049

p25v45 = T.mkSrcPos tDomainExpr 250045

p32v1 = T.mkSrcPos tDomainExpr 320001

p32v27 = T.mkSrcPos tDomainExpr 320027

p33v40 = T.mkSrcPos tDomainExpr 330040

p33v57 = T.mkSrcPos tDomainExpr 330057

p34v40 = T.mkSrcPos tDomainExpr 340040

p35v40 = T.mkSrcPos tDomainExpr 350040

p35v47 = T.mkSrcPos tDomainExpr 350047

p35v52 = T.mkSrcPos tDomainExpr 350052

p36v40 = T.mkSrcPos tDomainExpr 360040

p36v46 = T.mkSrcPos tDomainExpr 360046

p36v47 = T.mkSrcPos tDomainExpr 360047

p39v40 = T.mkSrcPos tDomainExpr 390040

p39v47 = T.mkSrcPos tDomainExpr 390047

p39v52 = T.mkSrcPos tDomainExpr 390052

p40v40 = T.mkSrcPos tDomainExpr 400040

p40v46 = T.mkSrcPos tDomainExpr 400046

p40v51 = T.mkSrcPos tDomainExpr 400051

p41v46 = T.mkSrcPos tDomainExpr 410046

p48v1 = T.mkSrcPos tDomainExpr 480001

p49v6 = T.mkSrcPos tDomainExpr 490006

p49v24 = T.mkSrcPos tDomainExpr 490024

p49v35 = T.mkSrcPos tDomainExpr 490035

p51v6 = T.mkSrcPos tDomainExpr 510006

p51v14 = T.mkSrcPos tDomainExpr 510014

p51v18 = T.mkSrcPos tDomainExpr 510018

p51v41 = T.mkSrcPos tDomainExpr 510041

p53v36 = T.mkSrcPos tDomainExpr 530036

p54v36 = T.mkSrcPos tDomainExpr 540036

p54v45 = T.mkSrcPos tDomainExpr 540045

p54v49 = T.mkSrcPos tDomainExpr 540049

p55v36 = T.mkSrcPos tDomainExpr 550036

p55v45 = T.mkSrcPos tDomainExpr 550045

p55v49 = T.mkSrcPos tDomainExpr 550049

p56v36 = T.mkSrcPos tDomainExpr 560036

p63v1 = T.mkSrcPos tDomainExpr 630001

p63v37 = T.mkSrcPos tDomainExpr 630037

p64v37 = T.mkSrcPos tDomainExpr 640037

p64v43 = T.mkSrcPos tDomainExpr 640043

p65v37 = T.mkSrcPos tDomainExpr 650037

p65v43 = T.mkSrcPos tDomainExpr 650043

p66v37 = T.mkSrcPos tDomainExpr 660037

p67v37 = T.mkSrcPos tDomainExpr 670037

p74v1 = T.mkSrcPos tDomainExpr 740001

p75v6 = T.mkSrcPos tDomainExpr 750006

p78v6 = T.mkSrcPos tDomainExpr 780006

p78v12 = T.mkSrcPos tDomainExpr 780012

p81v6 = T.mkSrcPos tDomainExpr 810006

p81v12 = T.mkSrcPos tDomainExpr 810012

p84v35 = T.mkSrcPos tDomainExpr 840035

p84v6 = T.mkSrcPos tDomainExpr 840006

p84v12 = T.mkSrcPos tDomainExpr 840012

p84v38 = T.mkSrcPos tDomainExpr 840038

p87v6 = T.mkSrcPos tDomainExpr 870006

p94v1 = T.mkSrcPos tDomainExpr 940001

p95v6 = T.mkSrcPos tDomainExpr 950006

p96v9 = T.mkSrcPos tDomainExpr 960009

p96v23 = T.mkSrcPos tDomainExpr 960023

p96v34 = T.mkSrcPos tDomainExpr 960034

p96v35 = T.mkSrcPos tDomainExpr 960035

p96v44 = T.mkSrcPos tDomainExpr 960044

p96v49 = T.mkSrcPos tDomainExpr 960049

p98v35 = T.mkSrcPos tDomainExpr 980035

p101v1 = T.mkSrcPos tDomainExpr 1010001

p102v6 = T.mkSrcPos tDomainExpr 1020006

p102v14 = T.mkSrcPos tDomainExpr 1020014

p105v47 = T.mkSrcPos tDomainExpr 1050047

p105v6 = T.mkSrcPos tDomainExpr 1050006

p105v57 = T.mkSrcPos tDomainExpr 1050057

p106v50 = T.mkSrcPos tDomainExpr 1060050

p106v6 = T.mkSrcPos tDomainExpr 1060006

p106v14 = T.mkSrcPos tDomainExpr 1060014

p106v16 = T.mkSrcPos tDomainExpr 1060016

p106v17 = T.mkSrcPos tDomainExpr 1060017

p106v41 = T.mkSrcPos tDomainExpr 1060041

p106v20 = T.mkSrcPos tDomainExpr 1060020

p106v21 = T.mkSrcPos tDomainExpr 1060021

p106v22 = T.mkSrcPos tDomainExpr 1060022

p106v60 = T.mkSrcPos tDomainExpr 1060060

p107v6 = T.mkSrcPos tDomainExpr 1070006

p107v15 = T.mkSrcPos tDomainExpr 1070015

p107v21 = T.mkSrcPos tDomainExpr 1070021

p107v22 = T.mkSrcPos tDomainExpr 1070022

p111v40 = T.mkSrcPos tDomainExpr 1110040

p111v6 = T.mkSrcPos tDomainExpr 1110006

p111v49 = T.mkSrcPos tDomainExpr 1110049

p112v6 = T.mkSrcPos tDomainExpr 1120006

p112v15 = T.mkSrcPos tDomainExpr 1120015

p116v40 = T.mkSrcPos tDomainExpr 1160040

p116v6 = T.mkSrcPos tDomainExpr 1160006

p116v49 = T.mkSrcPos tDomainExpr 1160049

p117v6 = T.mkSrcPos tDomainExpr 1170006

p117v15 = T.mkSrcPos tDomainExpr 1170015

p121v40 = T.mkSrcPos tDomainExpr 1210040

p121v6 = T.mkSrcPos tDomainExpr 1210006

p121v49 = T.mkSrcPos tDomainExpr 1210049

p122v40 = T.mkSrcPos tDomainExpr 1220040

p122v6 = T.mkSrcPos tDomainExpr 1220006

p122v49 = T.mkSrcPos tDomainExpr 1220049

p123v6 = T.mkSrcPos tDomainExpr 1230006

p123v15 = T.mkSrcPos tDomainExpr 1230015

p127v6 = T.mkSrcPos tDomainExpr 1270006

p127v12 = T.mkSrcPos tDomainExpr 1270012

p130v1 = T.mkSrcPos tDomainExpr 1300001

p131v6 = T.mkSrcPos tDomainExpr 1310006

p131v14 = T.mkSrcPos tDomainExpr 1310014

p134v40 = T.mkSrcPos tDomainExpr 1340040

p134v6 = T.mkSrcPos tDomainExpr 1340006

p134v50 = T.mkSrcPos tDomainExpr 1340050

p135v40 = T.mkSrcPos tDomainExpr 1350040

p135v6 = T.mkSrcPos tDomainExpr 1350006

p135v50 = T.mkSrcPos tDomainExpr 1350050

p136v6 = T.mkSrcPos tDomainExpr 1360006

p136v23 = T.mkSrcPos tDomainExpr 1360023

p140v6 = T.mkSrcPos tDomainExpr 1400006

p140v12 = T.mkSrcPos tDomainExpr 1400012
