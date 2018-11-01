module TConstructors
  (gcoMakeConstructorInstance,gcoCGen_aux,gcoCGen_aux_cross) where

import qualified Prelude 
import qualified Hat as T 
import qualified TPreludeBasic 
import TPrelude 
import TBaseDefs 
import TUtils 
import TMyUtils 
import TDomainExpr 
import TAbstractVals2 
import TSuccsAndPreds2 
import TAbstractMisc 
import TInverse 
import TApply 

gcoMakeConstructorInstance ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun Bool
          (T.Fun (T.List ConstrElem) (T.Fun DExpr (T.Fun DSubst Route))))

gcoMakeConstructorInstance pcoMakeConstructorInstance p =
  T.fun4 acoMakeConstructorInstance pcoMakeConstructorInstance p
    hcoMakeConstructorInstance
  where
  
  hcoMakeConstructorInstance fmi fcargs fsimplest_init fusage p =
    let
      gdoCAFkludge pdoCAFkludge p = T.constUse pdoCAFkludge p sdoCAFkludge
      gsimplest pdoCAFkludge p = T.constUse pdoCAFkludge p ssimplest
      j33v9doCAFkludge =
        case
          T.ccase p34v14 p
            (let
              v34v14v1 (fdx@(T.R (DXFunc _ _) _)) p =
                T.con2 p35v35 p T.Tuple2 T.aTuple2
                  (T.con0 p35v36 p False aFalse) fdx
              v34v14v1 fdx_CAF p =
                T.con2 p36v35 p T.Tuple2 T.aTuple2 (T.con0 p36v36 p True aTrue)
                  (T.con2 p36v42 p DXFunc aDXFunc
                    (T.con0 p36v49 p T.List T.aList) fdx_CAF) in (v34v14v1))
            fsimplest_init of
          T.R (T.Tuple2 fdoCAFkludge fsimplest) kdoCAFkludge ->
            (kdoCAFkludge,fdoCAFkludge,fsimplest)
          _ -> T.fatal p
      sdoCAFkludge =
        T.constDef p a33v10doCAFkludge
          (\ _ ->
            case j33v9doCAFkludge of
              (kdoCAFkludge,fdoCAFkludge,fsimplest) ->
                T.projection p33v10 kdoCAFkludge fdoCAFkludge)
      ssimplest =
        T.constDef p a33v23simplest
          (\ _ ->
            case j33v9doCAFkludge of
              (kdoCAFkludge,fdoCAFkludge,fsimplest) ->
                T.projection p33v23 kdoCAFkludge fsimplest)
      grecursive precursive p = T.constUse precursive p srecursive
      srecursive =
        T.constDef p a42v9recursive
          (\ p ->
            T.ccase p43v14 p
              (let
                v43v14v1 (T.R (DXFunc _ (T.R (DXLift1 _) _)) _) p =
                  T.con0 p44v42 p False aFalse
                v43v14v1 (T.R (DXFunc _ (T.R (DXLift2 _) _)) _) p =
                  T.con0 p45v42 p True aTrue
                v43v14v1 fanythingElse p =
                  T.ap1 p46v33 p (gpanic p46v33 p)
                    (T.fromLitString p46v39 p
                      "coMakeConstructorInstance:recursive") in (v43v14v1))
              (gsimplest p43v19 p))
      gactual pactual p = T.constUse pactual p sactual
      sactual =
        T.constDef p a48v9actual
          (\ p ->
            T.ap2 p49v14 p (gdxApplyDSubst p49v14 p) fusage
              (gsimplest p49v34 p))
      gactualSources pactualSources p =
        T.constUse pactualSources p sactualSources
      gactualTarget pactualSources p = T.constUse pactualSources p sactualTarget
      j51v9actualSources =
        case
          T.ccase p52v14 p
            (let
              v52v14v1 (T.R (Func fdss fdt) _) p =
                T.con2 p52v44 p T.Tuple2 T.aTuple2 fdss fdt
              v52v14v1 _ p = T.fatal p in (v52v14v1)) (gactual p52v19 p) of
          T.R (T.Tuple2 factualSources factualTarget) kactualSources ->
            (kactualSources,factualSources,factualTarget)
          _ -> T.fatal p
      sactualSources =
        T.constDef p a51v10actualSources
          (\ _ ->
            case j51v9actualSources of
              (kactualSources,factualSources,factualTarget) ->
                T.projection p51v10 kactualSources factualSources)
      sactualTarget =
        T.constDef p a51v25actualTarget
          (\ _ ->
            case j51v9actualSources of
              (kactualSources,factualSources,factualTarget) ->
                T.projection p51v25 kactualSources factualTarget)
      gtarget_domain_products ptarget_domain_products p =
        T.constUse ptarget_domain_products p starget_domain_products
      gpoints_below_structure_point ptarget_domain_products p =
        T.constUse ptarget_domain_products p spoints_below_structure_point
      j58v9target_domain_products =
        case
          T.ccase p59v14 p
            (let
              v59v14v1 (T.R (T.Tuple2 (T.R True _) (T.R (Lift2 fdts) _)) _) p =
                T.con2 p60v42 p T.Tuple2 T.aTuple2 fdts
                  (T.fromExpList p60v61 p
                    [T.con0 p60v62 p Stop2 aStop2,T.con0 p60v69 p Up2 aUp2])
              v59v14v1
                (T.R
                  (T.Tuple2 (T.R True _)
                    (T.R (Lift1 (T.R (T.Cons (T.R Two _) (T.R T.List _)) _)) _))
                  _) p =
                T.con2 p61v42 p T.Tuple2 T.aTuple2
                  (T.ap1 p61v43 p (gpanic p61v43 p)
                    (T.fromLitString p61v49 p "cMCI(1)"))
                  (T.fromExpList p61v61 p
                    [T.con0 p61v62 p Stop1 aStop1
                      ,T.con1 p61v69 p Up1 aUp1
                        (T.fromExpList p61v73 p [T.con0 p61v74 p Zero aZero])])
              v59v14v1 (T.R (T.Tuple2 (T.R False _) (T.R (Lift1 fdts) _)) _) p =
                T.con2 p62v42 p T.Tuple2 T.aTuple2 fdts
                  (T.fromExpList p62v61 p [T.con0 p62v62 p Stop1 aStop1])
              v59v14v1 (T.R (T.Tuple2 (T.R False _) (T.R Two _)) _) p =
                T.con2 p63v42 p T.Tuple2 T.aTuple2
                  (T.ap1 p63v43 p (gpanic p63v43 p)
                    (T.fromLitString p63v49 p "cMCI(2)"))
                  (T.fromExpList p63v61 p [T.con0 p63v62 p Zero aZero])
              v59v14v1 _ p = T.fatal p in (v59v14v1))
            (T.con2 p59v19 p T.Tuple2 T.aTuple2 (grecursive p59v20 p)
              (gactualTarget p59v31 p)) of
          T.R (T.Tuple2 ftarget_domain_products fpoints_below_structure_point)
              ktarget_domain_products ->
            (ktarget_domain_products,ftarget_domain_products
              ,fpoints_below_structure_point)
          _ -> T.fatal p
      starget_domain_products =
        T.constDef p a58v10target_domain_products
          (\ _ ->
            case j58v9target_domain_products of
              (ktarget_domain_products,ftarget_domain_products
                  ,fpoints_below_structure_point) ->
                T.projection p58v10 ktarget_domain_products
                  ftarget_domain_products)
      spoints_below_structure_point =
        T.constDef p a58v34points_below_structure_point
          (\ _ ->
            case j58v9target_domain_products of
              (ktarget_domain_products,ftarget_domain_products
                  ,fpoints_below_structure_point) ->
                T.projection p58v34 ktarget_domain_products
                  fpoints_below_structure_point)
      gall_product_points pall_product_points p =
        T.constUse pall_product_points p sall_product_points
      sall_product_points =
        T.constDef p a65v9all_product_points
          (\ p ->
            T.ap1 p66v14 p (gmyCartesianProduct p66v14 p)
              (T.ap2 p66v34 p (gmap p66v34 p) (gamAllRoutes p66v38 p)
                (gtarget_domain_products p66v50 p)))
      gpoints_not_below_structure_point ppoints_not_below_structure_point p =
        T.constUse ppoints_not_below_structure_point p
          spoints_not_below_structure_point
      spoints_not_below_structure_point =
        T.constDef p a68v9points_not_below_structure_point
          (\ p ->
            T.ccase p69v14 p
              (let
                v69v14v1 (T.R (T.Tuple2 (T.R True _) (T.R (Lift2 fdts) _)) _)
                  p =
                  T.ap2 p70v42 p (gmap p70v42 p)
                    (T.pa0 UpUp2 T.cn1 p70v46 p aUpUp2)
                    (gall_product_points p70v52 p)
                v69v14v1
                  (T.R
                    (T.Tuple2 (T.R True _)
                      (T.R (Lift1 (T.R (T.Cons (T.R Two _) (T.R T.List _)) _))
                        _)) _) p =
                  T.fromExpList p71v42 p
                    [T.con1 p71v43 p Up1 aUp1
                        (T.fromExpList p71v47 p [T.con0 p71v48 p One aOne])]
                v69v14v1 (T.R (T.Tuple2 (T.R False _) (T.R (Lift1 fdts) _)) _)
                  p =
                  T.ap2 p72v42 p (gmap p72v42 p) (T.pa0 Up1 T.cn1 p72v46 p aUp1)
                    (gall_product_points p72v52 p)
                v69v14v1 (T.R (T.Tuple2 (T.R False _) (T.R Two _)) _) p =
                  T.fromExpList p73v42 p [T.con0 p73v43 p One aOne]
                v69v14v1 _ p = T.fatal p in (v69v14v1))
              (T.con2 p69v19 p T.Tuple2 T.aTuple2 (grecursive p69v20 p)
                (gactualTarget p69v31 p)))
      gtagTable ptagTable p = T.constUse ptagTable p stagTable
      stagTable =
        T.constDef p a75v9tagTable
          (\ p ->
            T.ap2 p77v51 p (p77v51 !++ p)
              (T.ap1 p0v0 p
                (T.ap2 p76v14 p (TPrelude.g_foldr p76v14 p)
                  (T.fun2 T.mkLambda p76v14 p
                    (\ f_x f_y p ->
                      T.ccase p0v0 p
                        (let
                          v0v0v1 fp p =
                            T.ap1 p76v14 p
                              (T.pa1 T.Cons T.cn1 p76v14 p T.aCons
                                (T.con2 p76v15 p T.Tuple2 T.aTuple2 fp
                                  (garg_bottoms p76v19 p))) f_y
                          v0v0v1 _ p = T.projection p76v14 p f_y in (v0v0v1))
                        f_x)) (gpoints_below_structure_point p77v21 p))
                (T.fromExpList p0v0 p []))
              (T.ap1 p0v0 p
                (T.ap2 p78v14 p (TPrelude.g_foldr p78v14 p)
                  (T.fun2 T.mkLambda p78v14 p
                    (\ f_x f_y p ->
                      T.ccase p0v0 p
                        (let
                          v0v0v1 fp p =
                            T.ap1 p78v14 p
                              (T.pa1 T.Cons T.cn1 p78v14 p T.aCons
                                (T.con2 p78v15 p T.Tuple2 T.aTuple2 fp
                                  (T.fromExpList p78v19 p
                                    [T.con1 p78v20 p MkFrel aMkFrel
                                        (T.ap2 p78v28 p (gmap p78v28 p)
                                          (T.ap1 p78v33 p (gmagic p78v33 p) fp)
                                          fcargs)]))) f_y
                          v0v0v1 _ p = T.projection p78v14 p f_y in (v0v0v1))
                        f_x)) (gpoints_not_below_structure_point p79v21 p))
                (T.fromExpList p0v0 p [])))
      garg_bottoms parg_bottoms p = T.constUse parg_bottoms p sarg_bottoms
      sarg_bottoms =
        T.constDef p a81v9arg_bottoms
          (\ p ->
            T.fromExpList p82v14 p
              [T.con1 p82v15 p MkFrel aMkFrel
                  (T.ap2 p82v23 p (gmap p82v23 p) (gavBottomR p82v27 p)
                    (gactualSources p82v37 p))])
      gmagic pmagic p =
        T.fun2 a88v9magic pmagic p hmagic
        where
        
        hmagic fp (T.R ConstrRec _) p = T.projection p88v35 p fp
        hmagic fp (T.R (ConstrVar fn) _) p =
          T.ap2 p89v42 p (p89v42 !## p) (T.ap1 p89v35 p (gxpts p89v35 p) fp) fn
        hmagic _ _ p = T.fatal p
        
      gxpts pxpts p =
        T.fun1 a91v9xpts pxpts p hxpts
        where
        
        hxpts fp p =
          T.cguard p92v14 p (grecursive p92v14 p)
            (\ p ->
              T.ccase p92v28 p
                (let
                  v92v28v1 (T.R (UpUp2 frs) _) p = T.projection p92v50 p frs
                  v92v28v1 _ p = T.fatal p in (v92v28v1)) fp)
            (\ p ->
              T.cguard p93v14 p (gotherwise p93v14 p)
                (\ p ->
                  T.ccase p93v28 p
                    (let
                      v93v28v1 (T.R (Up1 frs) _) p = T.projection p93v50 p frs
                      v93v28v1 _ p = T.fatal p in (v93v28v1)) fp)
                (\ p -> T.fatal p))
         in
      (T.cif p100v9 p (gdoCAFkludge p100v15 p)
        (\ p ->
          T.ap1 p101v15 p (gapPapConst p101v15 p)
            (T.ap3 p101v27 p (gcoCGen_aux p101v27 p) fmi (gtagTable p101v41 p)
              (gactual p101v50 p)))
        (\ p ->
          T.con1 p102v15 p Rep aRep
            (T.ap3 p102v27 p (gcoCGen_aux p102v27 p) fmi (gtagTable p102v41 p)
              (gactual p102v50 p))))
  

gcoCGen_aux ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun Bool
          (T.Fun (AList Route (T.List FrontierElem)) (T.Fun Domain Rep)))

gcoCGen_aux pcoCGen_aux p =
  T.fun3 acoCGen_aux pcoCGen_aux p hcoCGen_aux
  where
  
  hcoCGen_aux fmi ftt (T.R (Func fdss (T.R Two _)) _) p =
    let
      gf1 pf1 p = T.constUse pf1 p sf1
      sf1 =
        T.constDef p a113v10f1
          (\ p ->
            T.ap1 p113v15 p (gsort p113v15 p)
              (T.ap3 p113v21 p (gutSureLookup p113v21 p) ftt
                (T.fromLitString p113v37 p "coCGen_aux(1)")
                (T.con0 p113v53 p One aOne)))
      gf0 pf0 p = T.constUse pf0 p sf0
      sf0 =
        T.constDef p a114v10f0
          (\ p ->
            T.ap2 p114v15 p (gspMax0FromMin1 p114v15 p) fdss (gf1 p114v34 p))
      gar par p = T.constUse par p sar
      sar =
        T.constDef p a115v10ar
          (\ p ->
            T.ccase p115v15 p
              (let
                v115v15v1 (T.R (MkFrel ffels) _) p =
                  T.ap1 p115v54 p (glength p115v54 p) ffels
                v115v15v1 _ p = T.fatal p in (v115v15v1))
              (T.ap1 p115v20 p (ghead p115v20 p)
                (T.ap2 p115v29 p (p115v29 !++ p) (gf1 p115v26 p)
                  (gf0 p115v32 p)))) in
      (T.con1 p116v10 p RepTwo aRepTwo
        (T.con3 p116v18 p Min1Max0 aMin1Max0 (gar p116v27 p) (gf1 p116v30 p)
          (gf0 p116v33 p)))
  hcoCGen_aux fmi ftt (T.R (Func fdss (T.R (Lift1 fdts) _)) _) p =
    let
      glf_f1 plf_f1 p = T.constUse plf_f1 p slf_f1
      slf_f1 =
        T.constDef p a119v10lf_f1
          (\ p ->
            T.ap1 p119v18 p (gsort p119v18 p)
              (T.ap3 p119v24 p (gutSureLookup p119v24 p) ftt
                (T.fromLitString p119v40 p "coCGen_aux(2)")
                (T.con1 p119v57 p Up1 aUp1
                  (T.ap2 p119v62 p (gmap p119v62 p) (gavBottomR p119v66 p)
                    fdts))))
      glf_f0 plf_f0 p = T.constUse plf_f0 p slf_f0
      slf_f0 =
        T.constDef p a120v10lf_f0
          (\ p ->
            T.ap2 p120v18 p (gspMax0FromMin1 p120v18 p) fdss (glf_f1 p120v37 p))
      glf_ar plf_ar p = T.constUse plf_ar p slf_ar
      slf_ar =
        T.constDef p a121v10lf_ar
          (\ p -> T.ap1 p121v18 p (glength p121v18 p) fdss)
      gnewtt pnewtt p = T.constUse pnewtt p snewtt
      snewtt =
        T.constDef p a122v10newtt
          (\ p ->
            T.ap1 p0v0 p
              (T.ap2 p122v18 p (TPrelude.g_foldr p122v18 p)
                (T.fun2 T.mkLambda p122v18 p
                  (\ f_x f_y p ->
                    T.ccase p0v0 p
                      (let
                        v0v0v1 (T.R (T.Tuple2 (T.R (Up1 frs) _) ffels) _) p =
                          T.ap1 p122v18 p
                            (T.pa1 T.Cons T.cn1 p122v18 p T.aCons
                              (T.con2 p122v19 p T.Tuple2 T.aTuple2 frs ffels))
                            f_y
                        v0v0v1 _ p = T.projection p122v18 p f_y in (v0v0v1))
                      f_x)) ftt) (T.fromExpList p0v0 p [])) in
      (T.con2 p124v10 p Rep1 aRep1
        (T.con3 p124v16 p Min1Max0 aMin1Max0 (glf_ar p124v25 p)
          (glf_f1 p124v31 p) (glf_f0 p124v37 p))
        (T.ap4 p125v16 p (gcoCGen_aux_cross p125v16 p) fmi (gnewtt p125v36 p)
          fdss fdts))
  hcoCGen_aux fmi ftt (T.R (Func fdss (T.R (Lift2 fdts) _)) _) p =
    let
      glf_f1 plf_f1 p = T.constUse plf_f1 p slf_f1
      slf_f1 =
        T.constDef p a128v10lf_f1
          (\ p ->
            T.ap1 p128v18 p (gsort p128v18 p)
              (T.ap3 p128v24 p (gutSureLookup p128v24 p) ftt
                (T.fromLitString p128v40 p "coCGen_aux(2)")
                (T.con0 p128v56 p Up2 aUp2)))
      glf_f0 plf_f0 p = T.constUse plf_f0 p slf_f0
      slf_f0 =
        T.constDef p a129v10lf_f0
          (\ p ->
            T.ap2 p129v18 p (gspMax0FromMin1 p129v18 p) fdss (glf_f1 p129v37 p))
      gmf_f1 pmf_f1 p = T.constUse pmf_f1 p smf_f1
      smf_f1 =
        T.constDef p a130v10mf_f1
          (\ p ->
            T.ap1 p130v18 p (gsort p130v18 p)
              (T.ap3 p130v24 p (gutSureLookup p130v24 p) ftt
                (T.fromLitString p130v40 p "coCGen_aux(3)")
                (T.con1 p130v57 p UpUp2 aUpUp2
                  (T.ap2 p130v64 p (gmap p130v64 p) (gavBottomR p130v68 p)
                    fdts))))
      gmf_f0 pmf_f0 p = T.constUse pmf_f0 p smf_f0
      smf_f0 =
        T.constDef p a131v10mf_f0
          (\ p ->
            T.ap2 p131v18 p (gspMax0FromMin1 p131v18 p) fdss (gmf_f1 p131v37 p))
      glf_ar plf_ar p = T.constUse plf_ar p slf_ar
      slf_ar =
        T.constDef p a132v10lf_ar
          (\ p -> T.ap1 p132v18 p (glength p132v18 p) fdss)
      gnewtt pnewtt p = T.constUse pnewtt p snewtt
      snewtt =
        T.constDef p a133v10newtt
          (\ p ->
            T.ap1 p0v0 p
              (T.ap2 p133v18 p (TPrelude.g_foldr p133v18 p)
                (T.fun2 T.mkLambda p133v18 p
                  (\ f_x f_y p ->
                    T.ccase p0v0 p
                      (let
                        v0v0v1 (T.R (T.Tuple2 (T.R (UpUp2 frs) _) ffels) _) p =
                          T.ap1 p133v18 p
                            (T.pa1 T.Cons T.cn1 p133v18 p T.aCons
                              (T.con2 p133v19 p T.Tuple2 T.aTuple2 frs ffels))
                            f_y
                        v0v0v1 _ p = T.projection p133v18 p f_y in (v0v0v1))
                      f_x)) ftt) (T.fromExpList p0v0 p [])) in
      (T.con3 p135v10 p Rep2 aRep2
        (T.con3 p135v16 p Min1Max0 aMin1Max0 (glf_ar p135v25 p)
          (glf_f1 p135v31 p) (glf_f0 p135v37 p))
        (T.con3 p136v16 p Min1Max0 aMin1Max0 (glf_ar p136v25 p)
          (gmf_f1 p136v31 p) (gmf_f0 p136v37 p))
        (T.ap4 p137v16 p (gcoCGen_aux_cross p137v16 p) fmi (gnewtt p137v36 p)
          fdss fdts))
  hcoCGen_aux fmi ftt (T.R (Func fdss (fgDomain@(T.R (Func fdss2 fdt) _))) _)
    p =
    let
      gnewtt pnewtt p = T.constUse pnewtt p snewtt
      snewtt =
        T.constDef p a140v10newtt
          (\ p ->
            T.ap2 p140v18 p (gmap p140v18 p) (gmakenewtt p140v22 p)
              (T.ap1 p140v33 p (gamAllRoutes p140v33 p) fdt))
      gmakenewtt pmakenewtt p =
        T.fun1 a141v10makenewtt pmakenewtt p hmakenewtt
        where
        
        hmakenewtt fx p =
          T.con2 p142v15 p T.Tuple2 T.aTuple2 fx
            (T.ap1 p143v16 p (gavMinfrel p143v16 p)
              (T.ap1 p0v0 p
                (T.ap2 p143v26 p (TPrelude.g_foldr p143v26 p)
                  (T.fun2 T.mkLambda p143v26 p
                    (\ f_x f_y p ->
                      T.ccase p0v0 p
                        (let
                          v0v0v1 (T.R (T.Tuple2 fg fmin_args_to_get_g) _) p =
                            T.ap1 p143v26 p
                              (T.ap2 p143v26 p (TPrelude.g_foldr p143v26 p)
                                (T.fun2 T.mkLambda p143v26 p
                                  (\ f_x f_y p ->
                                    T.ccase p0v0 p
                                      (let
                                        v0v0v1 (T.R (MkFrel fxs) _) p =
                                          T.ap1 p143v26 p
                                            (T.ap2 p143v26 p
                                              (TPrelude.g_foldr p143v26 p)
                                              (T.fun2 T.mkLambda p143v26 p
                                                (\ f_x f_y p ->
                                                  T.ccase p0v0 p
                                                    (let
                                                      v0v0v1
                                                        (T.R (MkFrel fys) _) p =
                                                        T.ap1 p143v26 p
                                                          (T.pa1 T.Cons T.cn1
                                                            p143v26 p T.aCons
                                                            (T.con1 p143v27 p
                                                              MkFrel aMkFrel
                                                              (T.ap2 p143v37 p
                                                                (p143v37 !++ p)
                                                                fxs fys))) f_y
                                                      v0v0v1 _ p =
                                                        T.projection p143v26 p
                                                          f_y in (v0v0v1)) f_x))
                                              (T.ap4 p146v42 p
                                                (ginMinInverse p146v42 p) fmi
                                                fgDomain fg fx)) f_y
                                        v0v0v1 _ p = T.projection p143v26 p f_y
                                        in (v0v0v1)) f_x)) fmin_args_to_get_g)
                              f_y
                          v0v0v1 _ p = T.projection p143v26 p f_y in (v0v0v1))
                        f_x)) ftt) (T.fromExpList p0v0 p [])))
         in
      (T.ap3 p148v10 p (gcoCGen_aux p148v10 p) fmi (gnewtt p148v24 p)
        (T.con2 p148v31 p Func aFunc
          (T.ap2 p148v40 p (p148v40 !++ p) fdss fdss2) fdt))
  hcoCGen_aux _ _ _ p = T.fatal p
  

gcoCGen_aux_cross ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun Bool
          (T.Fun (AList (T.List Route) (T.List FrontierElem))
            (T.Fun (T.List Domain) (T.Fun (T.List Domain) (T.List Rep)))))

gcoCGen_aux_cross pcoCGen_aux_cross p =
  T.fun4 acoCGen_aux_cross pcoCGen_aux_cross p hcoCGen_aux_cross
  where
  
  hcoCGen_aux_cross fmi ftt fdss fdts p =
    let
      gnumberOfDimensions pnumberOfDimensions p =
        T.constUse pnumberOfDimensions p snumberOfDimensions
      snumberOfDimensions =
        T.constDef p a160v10numberOfDimensions
          (\ p -> T.ap1 p161v15 p (glength p161v15 p) fdts)
      gdoOneDimension pdoOneDimension p =
        T.fun1 a162v10doOneDimension pdoOneDimension p hdoOneDimension
        where
        
        hdoOneDimension fn p =
          T.ap3 p163v15 p (gcoCGen_aux p163v15 p) fmi
            (T.ap1 p163v30 p (gfixtt p163v30 p) fn)
            (T.con2 p163v40 p Func aFunc fdss
              (T.ap2 p163v54 p (p163v54 !## p) fdts fn))
        
      gfixtt pfixtt p =
        T.fun1 a165v10fixtt pfixtt p hfixtt
        where
        
        hfixtt fn p =
          let
            gthisDimPoints pthisDimPoints p =
              T.constUse pthisDimPoints p sthisDimPoints
            sthisDimPoints =
              T.constDef p a166v19thisDimPoints
                (\ p ->
                  T.ap2 p167v24 p (gtaddall p167v24 p)
                    (T.con0 p167v32 p T.List T.aList) ftt)
            gtaddall ptaddall p =
              T.fun2 a169v19taddall ptaddall p htaddall
              where
              
              htaddall facc (T.R T.List _) p = T.projection p170v24 p facc
              htaddall facc (T.R (T.Cons (T.R (T.Tuple2 frs ffel) _) frest) _)
                p =
                T.ap2 p172v24 p (gtaddall p172v24 p)
                  (T.ap3 p172v33 p (gtadd p172v33 p)
                    (T.ap2 p172v42 p (p172v42 !## p) frs fn) ffel facc) frest
              htaddall _ _ p = T.fatal p
              
            gtadd ::
              T.RefSrcPos ->
                T.RefExp ->
                  T.R
                    (T.Fun Route
                      (T.Fun (T.List FrontierElem)
                        (T.Fun (AList Route (T.List (T.List FrontierElem)))
                          (AList Route (T.List (T.List FrontierElem))))))
            gtadd ptadd p =
              T.fun3 a178v19tadd ptadd p htadd
              where
              
              htadd fr ffel (T.R T.List _) p =
                T.fromExpList p179v24 p
                  [T.con2 p179v25 p T.Tuple2 T.aTuple2 fr
                      (T.fromExpList p179v29 p [ffel])]
              htadd fr ffel
                (T.R (T.Cons (fthis@(T.R (T.Tuple2 frr ffels) _)) frest) _) p =
                T.cguard p181v26 p (T.ap2 p181v26 p (p181v26 !== p) fr frr)
                  (\ p ->
                    T.con2 p181v51 p T.Cons T.aCons
                      (T.con2 p181v37 p T.Tuple2 T.aTuple2 frr
                        (T.con2 p181v45 p T.Cons T.aCons ffel ffels)) frest)
                  (\ p ->
                    T.cguard p182v24 p (gotherwise p182v24 p)
                      (\ p ->
                        T.con2 p182v42 p T.Cons T.aCons fthis
                          (T.ap3 p182v44 p (gtadd p182v44 p) fr ffel frest))
                      (\ p -> T.fatal p))
              htadd _ _ _ p = T.fatal p
              
            gfixedtt pfixedtt p = T.constUse pfixedtt p sfixedtt
            sfixedtt =
              T.constDef p a184v19fixedtt
                (\ p ->
                  T.ap2 p185v24 p (gmap2nd p185v24 p)
                    (T.ap2 p186v28 p (gfoldr p186v28 p)
                      (gavLUBmin1frontier p186v34 p)
                      (T.fromExpList p186v52 p
                        [T.con1 p186v53 p MkFrel aMkFrel
                            (T.ap2 p186v61 p (gmap p186v61 p)
                              (gavTopR p186v65 p) fdss)]))
                    (gthisDimPoints p187v27 p)) in (gfixedtt p189v19 p)
         in
      (T.ap2 p191v10 p (gmap p191v10 p) (gdoOneDimension p191v14 p)
        (T.ap2 p191v33 p (gmyIntsFromTo p191v33 p)
          (T.ap1 p191v30 p (TPreludeBasic.gfromInteger p191v30 p)
            (T.conInteger p191v30 p 0))
          (T.ap2 p191v66 p (p191v66 !- p) (gnumberOfDimensions p191v48 p)
            (T.ap1 p191v67 p (TPreludeBasic.gfromInteger p191v67 p)
              (T.conInteger p191v67 p 1)))))
  

tConstructors = T.mkModule "Constructors" "Constructors.hs" Prelude.True

acoMakeConstructorInstance =
  T.mkVariable tConstructors 260001 3 4 "coMakeConstructorInstance"
    Prelude.False

acoCGen_aux = T.mkVariable tConstructors 1120001 3 3 "coCGen_aux" Prelude.False

acoCGen_aux_cross =
  T.mkVariable tConstructors 1590001 3 4 "coCGen_aux_cross" Prelude.False

a33v10doCAFkludge =
  T.mkVariable tConstructors 330010 3 0 "doCAFkludge" Prelude.True

a33v23simplest = T.mkVariable tConstructors 330023 3 0 "simplest" Prelude.True

a42v9recursive = T.mkVariable tConstructors 420009 3 0 "recursive" Prelude.True

a48v9actual = T.mkVariable tConstructors 480009 3 0 "actual" Prelude.True

a51v10actualSources =
  T.mkVariable tConstructors 510010 3 0 "actualSources" Prelude.True

a51v25actualTarget =
  T.mkVariable tConstructors 510025 3 0 "actualTarget" Prelude.True

a58v10target_domain_products =
  T.mkVariable tConstructors 580010 3 0 "target_domain_products" Prelude.True

a58v34points_below_structure_point =
  T.mkVariable tConstructors 580034 3 0 "points_below_structure_point"
    Prelude.True

a65v9all_product_points =
  T.mkVariable tConstructors 650009 3 0 "all_product_points" Prelude.True

a68v9points_not_below_structure_point =
  T.mkVariable tConstructors 680009 3 0 "points_not_below_structure_point"
    Prelude.True

a75v9tagTable = T.mkVariable tConstructors 750009 3 0 "tagTable" Prelude.True

a81v9arg_bottoms =
  T.mkVariable tConstructors 810009 3 0 "arg_bottoms" Prelude.True

a88v9magic = T.mkVariable tConstructors 880009 3 2 "magic" Prelude.True

a91v9xpts = T.mkVariable tConstructors 910009 3 1 "xpts" Prelude.True

a113v10f1 = T.mkVariable tConstructors 1130010 3 0 "f1" Prelude.True

a114v10f0 = T.mkVariable tConstructors 1140010 3 0 "f0" Prelude.True

a115v10ar = T.mkVariable tConstructors 1150010 3 0 "ar" Prelude.True

a119v10lf_f1 = T.mkVariable tConstructors 1190010 3 0 "lf_f1" Prelude.True

a120v10lf_f0 = T.mkVariable tConstructors 1200010 3 0 "lf_f0" Prelude.True

a121v10lf_ar = T.mkVariable tConstructors 1210010 3 0 "lf_ar" Prelude.True

a122v10newtt = T.mkVariable tConstructors 1220010 3 0 "newtt" Prelude.True

a128v10lf_f1 = T.mkVariable tConstructors 1280010 3 0 "lf_f1" Prelude.True

a129v10lf_f0 = T.mkVariable tConstructors 1290010 3 0 "lf_f0" Prelude.True

a130v10mf_f1 = T.mkVariable tConstructors 1300010 3 0 "mf_f1" Prelude.True

a131v10mf_f0 = T.mkVariable tConstructors 1310010 3 0 "mf_f0" Prelude.True

a132v10lf_ar = T.mkVariable tConstructors 1320010 3 0 "lf_ar" Prelude.True

a133v10newtt = T.mkVariable tConstructors 1330010 3 0 "newtt" Prelude.True

a140v10newtt = T.mkVariable tConstructors 1400010 3 0 "newtt" Prelude.True

a141v10makenewtt =
  T.mkVariable tConstructors 1410010 3 1 "makenewtt" Prelude.True

a160v10numberOfDimensions =
  T.mkVariable tConstructors 1600010 3 0 "numberOfDimensions" Prelude.True

a162v10doOneDimension =
  T.mkVariable tConstructors 1620010 3 1 "doOneDimension" Prelude.True

a165v10fixtt = T.mkVariable tConstructors 1650010 3 1 "fixtt" Prelude.True

a166v19thisDimPoints =
  T.mkVariable tConstructors 1660019 3 0 "thisDimPoints" Prelude.True

a169v19taddall = T.mkVariable tConstructors 1690019 3 2 "taddall" Prelude.True

a178v19tadd = T.mkVariable tConstructors 1780019 3 3 "tadd" Prelude.True

a184v19fixedtt = T.mkVariable tConstructors 1840019 3 0 "fixedtt" Prelude.True

p26v1 = T.mkSrcPos tConstructors 260001

p33v10 = T.mkSrcPos tConstructors 330010

p33v23 = T.mkSrcPos tConstructors 330023

p34v14 = T.mkSrcPos tConstructors 340014

p35v35 = T.mkSrcPos tConstructors 350035

p35v36 = T.mkSrcPos tConstructors 350036

p36v35 = T.mkSrcPos tConstructors 360035

p36v36 = T.mkSrcPos tConstructors 360036

p36v42 = T.mkSrcPos tConstructors 360042

p36v49 = T.mkSrcPos tConstructors 360049

p42v9 = T.mkSrcPos tConstructors 420009

p43v14 = T.mkSrcPos tConstructors 430014

p43v19 = T.mkSrcPos tConstructors 430019

p44v42 = T.mkSrcPos tConstructors 440042

p45v42 = T.mkSrcPos tConstructors 450042

p46v33 = T.mkSrcPos tConstructors 460033

p46v39 = T.mkSrcPos tConstructors 460039

p48v9 = T.mkSrcPos tConstructors 480009

p49v14 = T.mkSrcPos tConstructors 490014

p49v34 = T.mkSrcPos tConstructors 490034

p51v10 = T.mkSrcPos tConstructors 510010

p51v25 = T.mkSrcPos tConstructors 510025

p52v14 = T.mkSrcPos tConstructors 520014

p52v19 = T.mkSrcPos tConstructors 520019

p52v44 = T.mkSrcPos tConstructors 520044

p58v10 = T.mkSrcPos tConstructors 580010

p58v34 = T.mkSrcPos tConstructors 580034

p59v14 = T.mkSrcPos tConstructors 590014

p59v19 = T.mkSrcPos tConstructors 590019

p59v20 = T.mkSrcPos tConstructors 590020

p59v31 = T.mkSrcPos tConstructors 590031

p60v42 = T.mkSrcPos tConstructors 600042

p60v61 = T.mkSrcPos tConstructors 600061

p60v62 = T.mkSrcPos tConstructors 600062

p60v69 = T.mkSrcPos tConstructors 600069

p61v42 = T.mkSrcPos tConstructors 610042

p61v43 = T.mkSrcPos tConstructors 610043

p61v49 = T.mkSrcPos tConstructors 610049

p61v61 = T.mkSrcPos tConstructors 610061

p61v62 = T.mkSrcPos tConstructors 610062

p61v69 = T.mkSrcPos tConstructors 610069

p61v73 = T.mkSrcPos tConstructors 610073

p61v74 = T.mkSrcPos tConstructors 610074

p62v42 = T.mkSrcPos tConstructors 620042

p62v61 = T.mkSrcPos tConstructors 620061

p62v62 = T.mkSrcPos tConstructors 620062

p63v42 = T.mkSrcPos tConstructors 630042

p63v43 = T.mkSrcPos tConstructors 630043

p63v49 = T.mkSrcPos tConstructors 630049

p63v61 = T.mkSrcPos tConstructors 630061

p63v62 = T.mkSrcPos tConstructors 630062

p65v9 = T.mkSrcPos tConstructors 650009

p66v14 = T.mkSrcPos tConstructors 660014

p66v34 = T.mkSrcPos tConstructors 660034

p66v38 = T.mkSrcPos tConstructors 660038

p66v50 = T.mkSrcPos tConstructors 660050

p68v9 = T.mkSrcPos tConstructors 680009

p69v14 = T.mkSrcPos tConstructors 690014

p69v19 = T.mkSrcPos tConstructors 690019

p69v20 = T.mkSrcPos tConstructors 690020

p69v31 = T.mkSrcPos tConstructors 690031

p70v42 = T.mkSrcPos tConstructors 700042

p70v46 = T.mkSrcPos tConstructors 700046

p70v52 = T.mkSrcPos tConstructors 700052

p71v42 = T.mkSrcPos tConstructors 710042

p71v43 = T.mkSrcPos tConstructors 710043

p71v47 = T.mkSrcPos tConstructors 710047

p71v48 = T.mkSrcPos tConstructors 710048

p72v42 = T.mkSrcPos tConstructors 720042

p72v46 = T.mkSrcPos tConstructors 720046

p72v52 = T.mkSrcPos tConstructors 720052

p73v42 = T.mkSrcPos tConstructors 730042

p73v43 = T.mkSrcPos tConstructors 730043

p75v9 = T.mkSrcPos tConstructors 750009

p77v51 = T.mkSrcPos tConstructors 770051

p0v0 = T.mkSrcPos tConstructors 0

p76v14 = T.mkSrcPos tConstructors 760014

p76v15 = T.mkSrcPos tConstructors 760015

p76v19 = T.mkSrcPos tConstructors 760019

p77v21 = T.mkSrcPos tConstructors 770021

p78v14 = T.mkSrcPos tConstructors 780014

p78v15 = T.mkSrcPos tConstructors 780015

p78v19 = T.mkSrcPos tConstructors 780019

p78v20 = T.mkSrcPos tConstructors 780020

p78v28 = T.mkSrcPos tConstructors 780028

p78v33 = T.mkSrcPos tConstructors 780033

p79v21 = T.mkSrcPos tConstructors 790021

p81v9 = T.mkSrcPos tConstructors 810009

p82v14 = T.mkSrcPos tConstructors 820014

p82v15 = T.mkSrcPos tConstructors 820015

p82v23 = T.mkSrcPos tConstructors 820023

p82v27 = T.mkSrcPos tConstructors 820027

p82v37 = T.mkSrcPos tConstructors 820037

p88v9 = T.mkSrcPos tConstructors 880009

p88v35 = T.mkSrcPos tConstructors 880035

p89v42 = T.mkSrcPos tConstructors 890042

p89v35 = T.mkSrcPos tConstructors 890035

p91v9 = T.mkSrcPos tConstructors 910009

p92v14 = T.mkSrcPos tConstructors 920014

p92v28 = T.mkSrcPos tConstructors 920028

p92v50 = T.mkSrcPos tConstructors 920050

p93v14 = T.mkSrcPos tConstructors 930014

p93v28 = T.mkSrcPos tConstructors 930028

p93v50 = T.mkSrcPos tConstructors 930050

p100v9 = T.mkSrcPos tConstructors 1000009

p100v15 = T.mkSrcPos tConstructors 1000015

p101v15 = T.mkSrcPos tConstructors 1010015

p101v27 = T.mkSrcPos tConstructors 1010027

p101v41 = T.mkSrcPos tConstructors 1010041

p101v50 = T.mkSrcPos tConstructors 1010050

p102v15 = T.mkSrcPos tConstructors 1020015

p102v27 = T.mkSrcPos tConstructors 1020027

p102v41 = T.mkSrcPos tConstructors 1020041

p102v50 = T.mkSrcPos tConstructors 1020050

p112v1 = T.mkSrcPos tConstructors 1120001

p113v10 = T.mkSrcPos tConstructors 1130010

p113v15 = T.mkSrcPos tConstructors 1130015

p113v21 = T.mkSrcPos tConstructors 1130021

p113v37 = T.mkSrcPos tConstructors 1130037

p113v53 = T.mkSrcPos tConstructors 1130053

p114v10 = T.mkSrcPos tConstructors 1140010

p114v15 = T.mkSrcPos tConstructors 1140015

p114v34 = T.mkSrcPos tConstructors 1140034

p115v10 = T.mkSrcPos tConstructors 1150010

p115v15 = T.mkSrcPos tConstructors 1150015

p115v20 = T.mkSrcPos tConstructors 1150020

p115v29 = T.mkSrcPos tConstructors 1150029

p115v26 = T.mkSrcPos tConstructors 1150026

p115v32 = T.mkSrcPos tConstructors 1150032

p115v54 = T.mkSrcPos tConstructors 1150054

p116v10 = T.mkSrcPos tConstructors 1160010

p116v18 = T.mkSrcPos tConstructors 1160018

p116v27 = T.mkSrcPos tConstructors 1160027

p116v30 = T.mkSrcPos tConstructors 1160030

p116v33 = T.mkSrcPos tConstructors 1160033

p119v10 = T.mkSrcPos tConstructors 1190010

p119v18 = T.mkSrcPos tConstructors 1190018

p119v24 = T.mkSrcPos tConstructors 1190024

p119v40 = T.mkSrcPos tConstructors 1190040

p119v57 = T.mkSrcPos tConstructors 1190057

p119v62 = T.mkSrcPos tConstructors 1190062

p119v66 = T.mkSrcPos tConstructors 1190066

p120v10 = T.mkSrcPos tConstructors 1200010

p120v18 = T.mkSrcPos tConstructors 1200018

p120v37 = T.mkSrcPos tConstructors 1200037

p121v10 = T.mkSrcPos tConstructors 1210010

p121v18 = T.mkSrcPos tConstructors 1210018

p122v10 = T.mkSrcPos tConstructors 1220010

p122v18 = T.mkSrcPos tConstructors 1220018

p122v19 = T.mkSrcPos tConstructors 1220019

p124v10 = T.mkSrcPos tConstructors 1240010

p124v16 = T.mkSrcPos tConstructors 1240016

p124v25 = T.mkSrcPos tConstructors 1240025

p124v31 = T.mkSrcPos tConstructors 1240031

p124v37 = T.mkSrcPos tConstructors 1240037

p125v16 = T.mkSrcPos tConstructors 1250016

p125v36 = T.mkSrcPos tConstructors 1250036

p128v10 = T.mkSrcPos tConstructors 1280010

p128v18 = T.mkSrcPos tConstructors 1280018

p128v24 = T.mkSrcPos tConstructors 1280024

p128v40 = T.mkSrcPos tConstructors 1280040

p128v56 = T.mkSrcPos tConstructors 1280056

p129v10 = T.mkSrcPos tConstructors 1290010

p129v18 = T.mkSrcPos tConstructors 1290018

p129v37 = T.mkSrcPos tConstructors 1290037

p130v10 = T.mkSrcPos tConstructors 1300010

p130v18 = T.mkSrcPos tConstructors 1300018

p130v24 = T.mkSrcPos tConstructors 1300024

p130v40 = T.mkSrcPos tConstructors 1300040

p130v57 = T.mkSrcPos tConstructors 1300057

p130v64 = T.mkSrcPos tConstructors 1300064

p130v68 = T.mkSrcPos tConstructors 1300068

p131v10 = T.mkSrcPos tConstructors 1310010

p131v18 = T.mkSrcPos tConstructors 1310018

p131v37 = T.mkSrcPos tConstructors 1310037

p132v10 = T.mkSrcPos tConstructors 1320010

p132v18 = T.mkSrcPos tConstructors 1320018

p133v10 = T.mkSrcPos tConstructors 1330010

p133v18 = T.mkSrcPos tConstructors 1330018

p133v19 = T.mkSrcPos tConstructors 1330019

p135v10 = T.mkSrcPos tConstructors 1350010

p135v16 = T.mkSrcPos tConstructors 1350016

p135v25 = T.mkSrcPos tConstructors 1350025

p135v31 = T.mkSrcPos tConstructors 1350031

p135v37 = T.mkSrcPos tConstructors 1350037

p136v16 = T.mkSrcPos tConstructors 1360016

p136v25 = T.mkSrcPos tConstructors 1360025

p136v31 = T.mkSrcPos tConstructors 1360031

p136v37 = T.mkSrcPos tConstructors 1360037

p137v16 = T.mkSrcPos tConstructors 1370016

p137v36 = T.mkSrcPos tConstructors 1370036

p140v10 = T.mkSrcPos tConstructors 1400010

p140v18 = T.mkSrcPos tConstructors 1400018

p140v22 = T.mkSrcPos tConstructors 1400022

p140v33 = T.mkSrcPos tConstructors 1400033

p141v10 = T.mkSrcPos tConstructors 1410010

p142v15 = T.mkSrcPos tConstructors 1420015

p143v16 = T.mkSrcPos tConstructors 1430016

p143v26 = T.mkSrcPos tConstructors 1430026

p143v27 = T.mkSrcPos tConstructors 1430027

p143v37 = T.mkSrcPos tConstructors 1430037

p146v42 = T.mkSrcPos tConstructors 1460042

p148v10 = T.mkSrcPos tConstructors 1480010

p148v24 = T.mkSrcPos tConstructors 1480024

p148v31 = T.mkSrcPos tConstructors 1480031

p148v40 = T.mkSrcPos tConstructors 1480040

p159v1 = T.mkSrcPos tConstructors 1590001

p160v10 = T.mkSrcPos tConstructors 1600010

p161v15 = T.mkSrcPos tConstructors 1610015

p162v10 = T.mkSrcPos tConstructors 1620010

p163v15 = T.mkSrcPos tConstructors 1630015

p163v30 = T.mkSrcPos tConstructors 1630030

p163v40 = T.mkSrcPos tConstructors 1630040

p163v54 = T.mkSrcPos tConstructors 1630054

p165v10 = T.mkSrcPos tConstructors 1650010

p166v19 = T.mkSrcPos tConstructors 1660019

p167v24 = T.mkSrcPos tConstructors 1670024

p167v32 = T.mkSrcPos tConstructors 1670032

p169v19 = T.mkSrcPos tConstructors 1690019

p170v24 = T.mkSrcPos tConstructors 1700024

p172v24 = T.mkSrcPos tConstructors 1720024

p172v33 = T.mkSrcPos tConstructors 1720033

p172v42 = T.mkSrcPos tConstructors 1720042

p178v19 = T.mkSrcPos tConstructors 1780019

p179v24 = T.mkSrcPos tConstructors 1790024

p179v25 = T.mkSrcPos tConstructors 1790025

p179v29 = T.mkSrcPos tConstructors 1790029

p181v26 = T.mkSrcPos tConstructors 1810026

p181v51 = T.mkSrcPos tConstructors 1810051

p181v37 = T.mkSrcPos tConstructors 1810037

p181v45 = T.mkSrcPos tConstructors 1810045

p182v24 = T.mkSrcPos tConstructors 1820024

p182v42 = T.mkSrcPos tConstructors 1820042

p182v44 = T.mkSrcPos tConstructors 1820044

p184v19 = T.mkSrcPos tConstructors 1840019

p185v24 = T.mkSrcPos tConstructors 1850024

p186v28 = T.mkSrcPos tConstructors 1860028

p186v34 = T.mkSrcPos tConstructors 1860034

p186v52 = T.mkSrcPos tConstructors 1860052

p186v53 = T.mkSrcPos tConstructors 1860053

p186v61 = T.mkSrcPos tConstructors 1860061

p186v65 = T.mkSrcPos tConstructors 1860065

p187v27 = T.mkSrcPos tConstructors 1870027

p189v19 = T.mkSrcPos tConstructors 1890019

p191v10 = T.mkSrcPos tConstructors 1910010

p191v14 = T.mkSrcPos tConstructors 1910014

p191v33 = T.mkSrcPos tConstructors 1910033

p191v30 = T.mkSrcPos tConstructors 1910030

p191v66 = T.mkSrcPos tConstructors 1910066

p191v48 = T.mkSrcPos tConstructors 1910048

p191v67 = T.mkSrcPos tConstructors 1910067
