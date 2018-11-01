module TFrontierGENERIC2
  (gfsMakeFrontierRep,gfsFind,gfsApp,gfsEvalConst,gfsFs2,gfsFs_aux) where

import qualified Prelude 
import qualified Hat as T 
import qualified TPreludeBasic 
import TPrelude 
import TBaseDefs 
import TUtils 
import TMyUtils 
import TAbstractVals2 
import TSuccsAndPreds2 
import TAbstractEval2 
import TAbsConc3 
import TFrontierMisc2 
import TFrontierDATAFN2 
import TAbstractMisc 
import TApply 

gfsMakeFrontierRep ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun ACMode
          (T.Fun Bool
            (T.Fun (HExpr Naam)
              (T.Fun Domain
                (T.Fun (T.List Domain)
                  (T.Fun Route (T.Fun Route (T.Tuple2 Route Int))))))))

gfsMakeFrontierRep pfsMakeFrontierRep p =
  T.fun7 afsMakeFrontierRep pfsMakeFrontierRep p hfsMakeFrontierRep
  where
  
  hfsMakeFrontierRep fs_or_l fnaive fhexpr ffunc_domain fbig_arg_ds
    flower_boundR fupper_boundR p =
    let
      gis_caf pis_caf p = T.constUse pis_caf p sis_caf
      gsmall_arg_ds pis_caf p = T.constUse pis_caf p ssmall_arg_ds
      j36v10is_caf =
        case
          T.ccase p37v15 p
            (let
              v37v15v1 (T.R (Func (T.R T.List _) fdt) _) p =
                T.con2 p38v39 p T.Tuple2 T.aTuple2 (T.con0 p38v40 p True aTrue)
                  (T.ap1 p38v46 p (gpanic p38v46 p)
                    (T.fromLitString p38v52 p "fsMakeFrontierRep(1)"))
              v37v15v1 (T.R (Func fdss fdt) _) p =
                T.con2 p39v39 p T.Tuple2 T.aTuple2
                  (T.con0 p39v40 p False aFalse) fdss
              v37v15v1 fnon_func_domain p =
                T.con2 p40v39 p T.Tuple2 T.aTuple2 (T.con0 p40v40 p True aTrue)
                  (T.ap1 p40v46 p (gpanic p40v46 p)
                    (T.fromLitString p40v52 p "fsMakeFrontierRep(2)")) in
              (v37v15v1)) ffunc_domain of
          T.R (T.Tuple2 fis_caf fsmall_arg_ds) kis_caf ->
            (kis_caf,fis_caf,fsmall_arg_ds)
          _ -> T.fatal p
      sis_caf =
        T.constDef p a36v11is_caf
          (\ _ ->
            case j36v10is_caf of
              (kis_caf,fis_caf,fsmall_arg_ds) ->
                T.projection p36v11 kis_caf fis_caf)
      ssmall_arg_ds =
        T.constDef p a36v19small_arg_ds
          (\ _ ->
            case j36v10is_caf of
              (kis_caf,fis_caf,fsmall_arg_ds) ->
                T.projection p36v19 kis_caf fsmall_arg_ds)
      ggetRep pgetRep p =
        T.fun1 a41v10getRep pgetRep p hgetRep
        where
        
        hgetRep (T.R (Rep frep) _) p = T.projection p42v15 p frep
        hgetRep _ p = T.fatal p
        
      gupper_bound pupper_bound p = T.constUse pupper_bound p supper_bound
      supper_bound =
        T.constDef p a43v10upper_bound
          (\ p -> T.ap1 p44v15 p (ggetRep p44v15 p) fupper_boundR)
      glower_bound plower_bound p = T.constUse plower_bound p slower_bound
      slower_bound =
        T.constDef p a45v10lower_bound
          (\ p -> T.ap1 p46v15 p (ggetRep p46v15 p) flower_boundR)
      gbound_rep pbound_rep p = T.constUse pbound_rep p sbound_rep
      sbound_rep =
        T.constDef p a47v10bound_rep
          (\ p ->
            T.ap2 p48v15 p (gfsZULB p48v15 p) (gupper_bound p48v22 p)
              (glower_bound p48v34 p))
      ginit_memo pinit_memo p = T.constUse pinit_memo p sinit_memo
      sinit_memo =
        T.constDef p a49v10init_memo (\ p -> T.con0 p50v15 p T.List T.aList)
      gcaf_result pcaf_result p = T.constUse pcaf_result p scaf_result
      scaf_result =
        T.constDef p a51v10caf_result
          (\ p -> T.ap1 p52v15 p (gaeEvalConst p52v15 p) fhexpr)
      gnon_data_fn_result pnon_data_fn_result p =
        T.constUse pnon_data_fn_result p snon_data_fn_result
      snon_data_fn_result =
        T.constDef p a53v10non_data_fn_result
          (\ p ->
            T.ap9 p54v15 p (gfsFind p54v15 p) fs_or_l fhexpr ffunc_domain
              (gsmall_arg_ds p55v22 p) fbig_arg_ds (gbound_rep p55v46 p)
              (T.ap1 p55v56 p (TPreludeBasic.gfromInteger p55v56 p)
                (T.conInteger p55v56 p 0)) (T.con0 p55v58 p T.List T.aList)
              fnaive)
      gdata_fn_result pdata_fn_result p =
        T.constUse pdata_fn_result p sdata_fn_result
      gfinal_memo pdata_fn_result p = T.constUse pdata_fn_result p sfinal_memo
      j56v10data_fn_result =
        case
          T.ap10 p57v15 p (gfdFind p57v15 p) fs_or_l fhexpr ffunc_domain
            (gsmall_arg_ds p58v22 p) fbig_arg_ds (gbound_rep p58v46 p)
            (gfdIdent p58v56 p) fnaive
            (T.ap1 p59v23 p (gpanic p59v23 p)
              (T.fromLitString p59v29 p "no inherited min1"))
            (ginit_memo p59v50 p) of
          T.R (T.Tuple2 fdata_fn_result ffinal_memo) kdata_fn_result ->
            (kdata_fn_result,fdata_fn_result,ffinal_memo)
          _ -> T.fatal p
      sdata_fn_result =
        T.constDef p a56v11data_fn_result
          (\ _ ->
            case j56v10data_fn_result of
              (kdata_fn_result,fdata_fn_result,ffinal_memo) ->
                T.projection p56v11 kdata_fn_result fdata_fn_result)
      sfinal_memo =
        T.constDef p a56v27final_memo
          (\ _ ->
            case j56v10data_fn_result of
              (kdata_fn_result,fdata_fn_result,ffinal_memo) ->
                T.projection p56v27 kdata_fn_result ffinal_memo)
      gdata_fn_evals pdata_fn_evals p =
        T.constUse pdata_fn_evals p sdata_fn_evals
      sdata_fn_evals =
        T.constDef p a60v10data_fn_evals
          (\ p -> T.ap1 p61v15 p (glength p61v15 p) (gfinal_memo p61v22 p))
      gcaf_result_norm pcaf_result_norm p =
        T.constUse pcaf_result_norm p scaf_result_norm
      scaf_result_norm =
        T.constDef p a62v10caf_result_norm
          (\ p ->
            T.ccase p63v15 p
              (let
                v63v15v1 (T.R (Rep frep) _) p =
                  T.ap1 p63v46 p (gapPapConst p63v46 p) frep
                v63v15v1 fother p = T.projection p63v71 p fother in (v63v15v1))
              (gcaf_result p63v20 p))
      gis_data_fn pis_data_fn p = T.constUse pis_data_fn p sis_data_fn
      sis_data_fn =
        T.constDef p a64v10is_data_fn
          (\ p -> T.ap1 p65v15 p (gamIsDataFn p65v15 p) ffunc_domain) in
      (T.cif p67v10 p (gis_caf p67v17 p)
        (\ p ->
          T.con2 p68v17 p T.Tuple2 T.aTuple2 (gcaf_result_norm p68v18 p)
            (T.ap1 p68v35 p (TPreludeBasic.gfromInteger p68v35 p)
              (T.conInteger p68v35 p 0)))
        (\ p ->
          T.cif p70v10 p (gis_data_fn p70v17 p)
            (\ p ->
              T.con2 p71v17 p T.Tuple2 T.aTuple2
                (T.con1 p71v18 p Rep aRep (gdata_fn_result p71v22 p))
                (gdata_fn_evals p71v38 p))
            (\ p ->
              T.con2 p72v17 p T.Tuple2 T.aTuple2
                (T.con1 p72v18 p Rep aRep (gnon_data_fn_result p72v22 p))
                (T.ap1 p72v43 p (TPrelude.gnegate p72v43 p)
                  (T.ap1 p72v44 p (TPreludeBasic.gfromInteger p72v44 p)
                    (T.conInteger p72v44 p 1))))))
  

gfsFind ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun ACMode
          (T.Fun (HExpr Naam)
            (T.Fun Domain
              (T.Fun (T.List Domain)
                (T.Fun (T.List Domain)
                  (T.Fun Rep
                    (T.Fun Int (T.Fun (T.List AppInfo) (T.Fun Bool Rep)))))))))

gfsFind pfsFind p =
  T.fun9 afsFind pfsFind p hfsFind
  where
  
  hfsFind fs_or_l fhexpr (T.R (Func fdss (T.R Two _)) _) fsmall_argds fbig_argds
    (T.R (RepTwo fbounds) _) fn fas fnaive p =
    T.con1 p97v6 p RepTwo aRepTwo
      (T.ap7 p97v14 p (gfsFs2 p97v14 p) fs_or_l fhexpr fsmall_argds fbig_argds
        fbounds
        (T.ap2 p102v23 p (p102v23 !++ p) fas
          (T.fromExpList p102v25 p [T.con0 p102v26 p A2 aA2])) fnaive)
  hfsFind fs_or_l fhexpr (T.R (Func fdss (T.R (Lift1 fdts) _)) _) fsmall_argds
    fbig_argds (T.R (Rep1 fbounds_lf fbounds_hfs) _) fn fas fnaive p =
    let
      glofact plofact p = T.constUse plofact p slofact
      slofact =
        T.constDef p a115v10lofact
          (\ p ->
            T.ap7 p116v15 p (gfsFs2 p116v15 p) fs_or_l fhexpr fsmall_argds
              fbig_argds fbounds_lf
              (T.ap2 p121v24 p (p121v24 !++ p) fas
                (T.fromExpList p121v26 p [T.con0 p121v27 p ALo1 aALo1])) fnaive)
      ghifact_ds phifact_ds p = T.constUse phifact_ds p shifact_ds
      shifact_ds =
        T.constDef p a123v10hifact_ds
          (\ p ->
            T.ap2 p124v15 p (gmap p124v15 p)
              (T.ap1 p124v20 p (gavUncurry p124v20 p) fdss) fdts)
      glofact_arity plofact_arity p = T.constUse plofact_arity p slofact_arity
      slofact_arity =
        T.constDef p a125v10lofact_arity
          (\ p -> T.ap1 p126v15 p (glength p126v15 p) fdss)
      ghifacts phifacts p = T.constUse phifacts p shifacts
      shifacts =
        T.constDef p a127v10hifacts
          (\ p ->
            T.ap5 p128v15 p (gmyZipWith4 p128v15 p) (gdoOne p128v26 p)
              (ghifact_ds p129v26 p) fdts fbounds_hfs
              (T.ap2 p132v30 p (gmyIntsFromTo p132v30 p)
                (T.ap1 p132v27 p (TPreludeBasic.gfromInteger p132v27 p)
                  (T.conInteger p132v27 p 0))
                (T.ap2 p132v56 p (p132v56 !- p)
                  (T.ap1 p132v45 p (glength p132v45 p) fdts)
                  (T.ap1 p132v58 p (TPreludeBasic.gfromInteger p132v58 p)
                    (T.conInteger p132v58 p 1)))))
      gdoOne pdoOne p =
        T.fun4 a133v10doOne pdoOne p hdoOne
        where
        
        hdoOne fhifact_d fhifact_targ_domain fbounds fnn p =
          T.ap9 p134v15 p (gfsFind p134v15 p) fs_or_l fhexpr fhifact_d
            fsmall_argds fbig_argds fbounds (glofact_arity p140v22 p)
            (T.ap2 p141v25 p (p141v25 !++ p) fas
              (T.fromExpList p141v27 p
                [T.con3 p141v28 p AHi1 aAHi1 (glofact_arity p141v33 p) fnn
                    fhifact_targ_domain])) fnaive
         in
      (T.con2 p144v10 p Rep1 aRep1 (glofact p144v15 p) (ghifacts p144v22 p))
  hfsFind fs_or_l fhexpr (T.R (Func fdss (T.R (Lift2 fdts) _)) _) fsmall_argds
    fbig_argds (T.R (Rep2 fbounds_lf fbounds_mf fbounds_hfs) _) fn fas fnaive
    p =
    let
      glofact plofact p = T.constUse plofact p slofact
      slofact =
        T.constDef p a156v10lofact
          (\ p ->
            T.ap7 p157v15 p (gfsFs2 p157v15 p) fs_or_l fhexpr fsmall_argds
              fbig_argds fbounds_lf
              (T.ap2 p162v24 p (p162v24 !++ p) fas
                (T.fromExpList p162v26 p [T.con0 p162v27 p ALo2 aALo2])) fnaive)
      gmidfact pmidfact p = T.constUse pmidfact p smidfact
      smidfact =
        T.constDef p a164v10midfact
          (\ p ->
            T.ap7 p165v15 p (gfsFs2 p165v15 p) fs_or_l fhexpr fsmall_argds
              fbig_argds fbounds_mf
              (T.ap2 p170v24 p (p170v24 !++ p) fas
                (T.fromExpList p170v26 p [T.con0 p170v27 p AMid2 aAMid2]))
              fnaive)
      ghifact_ds phifact_ds p = T.constUse phifact_ds p shifact_ds
      shifact_ds =
        T.constDef p a172v10hifact_ds
          (\ p ->
            T.ap2 p173v15 p (gmap p173v15 p)
              (T.ap1 p173v20 p (gavUncurry p173v20 p) fdss) fdts)
      glofact_arity plofact_arity p = T.constUse plofact_arity p slofact_arity
      slofact_arity =
        T.constDef p a174v10lofact_arity
          (\ p -> T.ap1 p175v15 p (glength p175v15 p) fdss)
      ghifacts phifacts p = T.constUse phifacts p shifacts
      shifacts =
        T.constDef p a176v10hifacts
          (\ p ->
            T.ap5 p177v15 p (gmyZipWith4 p177v15 p) (gdoOne p177v26 p)
              (ghifact_ds p178v26 p) fdts fbounds_hfs
              (T.ap2 p181v30 p (gmyIntsFromTo p181v30 p)
                (T.ap1 p181v27 p (TPreludeBasic.gfromInteger p181v27 p)
                  (T.conInteger p181v27 p 0))
                (T.ap2 p181v56 p (p181v56 !- p)
                  (T.ap1 p181v45 p (glength p181v45 p) fdts)
                  (T.ap1 p181v58 p (TPreludeBasic.gfromInteger p181v58 p)
                    (T.conInteger p181v58 p 1)))))
      gdoOne pdoOne p =
        T.fun4 a182v10doOne pdoOne p hdoOne
        where
        
        hdoOne fhifact_d fhifact_targ_domain fbounds fnn p =
          T.ap9 p183v15 p (gfsFind p183v15 p) fs_or_l fhexpr fhifact_d
            fsmall_argds fbig_argds fbounds (glofact_arity p189v22 p)
            (T.ap2 p190v25 p (p190v25 !++ p) fas
              (T.fromExpList p190v27 p
                [T.con3 p190v28 p AHi2 aAHi2 (glofact_arity p190v33 p) fnn
                    fhifact_targ_domain])) fnaive
         in
      (T.con3 p193v10 p Rep2 aRep2 (glofact p193v15 p) (gmidfact p193v22 p)
        (ghifacts p193v30 p))
  hfsFind _ _ _ _ _ _ _ _ _ p = T.fatal p
  

gfsApp ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.List AppInfo)
          (T.Fun (T.List (HExpr Naam)) (T.Fun (HExpr Naam) Route)))

gfsApp pfsApp p =
  T.fun3 afsApp pfsApp p hfsApp
  where
  
  hfsApp (T.R (T.Cons (T.R A2 _) (T.R T.List _)) _) fxs fh p =
    T.ap2 p204v6 p (gfsEvalConst p204v6 p) fh fxs
  hfsApp (T.R (T.Cons (T.R ALo1 _) (T.R T.List _)) _) fxs fh p =
    T.ccase p207v6 p
      (let
        v207v6v1 (T.R Stop1 _) p = T.con0 p208v19 p Zero aZero
        v207v6v1 (T.R (Up1 _) _) p = T.con0 p209v19 p One aOne
        v207v6v1 _ p = T.fatal p in (v207v6v1))
      (T.ap2 p207v11 p (gfsEvalConst p207v11 p) fh fxs)
  hfsApp (T.R (T.Cons (T.R (AHi1 fn fx fd) _) fas) _) fxs fh p =
    let
      gapp_res papp_res p = T.constUse papp_res p sapp_res
      sapp_res =
        T.constDef p a212v10app_res
          (\ p ->
            T.ap2 p212v26 p (gfsEvalConst p212v26 p) fh
              (T.ap2 p212v41 p (gtake p212v41 p) fn fxs))
      gnth_upp_obj pnth_upp_obj p = T.constUse pnth_upp_obj p snth_upp_obj
      snth_upp_obj =
        T.constDef p a213v10nth_upp_obj
          (\ p ->
            T.ccase p213v26 p
              (let
                v213v26v1 (T.R Stop1 _) p =
                  T.ap1 p214v40 p (gavBottomR p214v40 p) fd
                v213v26v1 (T.R (Up1 frs) _) p =
                  T.ap2 p215v43 p (p215v43 !## p) frs fx
                v213v26v1 _ p = T.fatal p in (v213v26v1)) (gapp_res p213v31 p))
      in
      (T.ap3 p217v10 p (gfsApp p217v10 p) fas
        (T.ap2 p217v20 p (gdrop p217v20 p) fn fxs)
        (T.con1 p217v32 p HPoint aHPoint (gnth_upp_obj p217v39 p)))
  hfsApp (T.R (T.Cons (T.R ALo2 _) (T.R T.List _)) _) fxs fh p =
    T.ccase p220v6 p
      (let
        v220v6v1 (T.R Stop2 _) p = T.con0 p221v21 p Zero aZero
        v220v6v1 (T.R Up2 _) p = T.con0 p222v21 p One aOne
        v220v6v1 (T.R (UpUp2 _) _) p = T.con0 p223v21 p One aOne
        v220v6v1 _ p = T.fatal p in (v220v6v1))
      (T.ap2 p220v11 p (gfsEvalConst p220v11 p) fh fxs)
  hfsApp (T.R (T.Cons (T.R AMid2 _) (T.R T.List _)) _) fxs fh p =
    T.ccase p226v6 p
      (let
        v226v6v1 (T.R Stop2 _) p = T.con0 p227v21 p Zero aZero
        v226v6v1 (T.R Up2 _) p = T.con0 p228v21 p Zero aZero
        v226v6v1 (T.R (UpUp2 _) _) p = T.con0 p229v21 p One aOne
        v226v6v1 _ p = T.fatal p in (v226v6v1))
      (T.ap2 p226v11 p (gfsEvalConst p226v11 p) fh fxs)
  hfsApp (T.R (T.Cons (T.R (AHi2 fn fx fd) _) fas) _) fxs fh p =
    let
      gapp_res papp_res p = T.constUse papp_res p sapp_res
      sapp_res =
        T.constDef p a232v10app_res
          (\ p ->
            T.ap2 p232v26 p (gfsEvalConst p232v26 p) fh
              (T.ap2 p232v41 p (gtake p232v41 p) fn fxs))
      gnth_upp_obj pnth_upp_obj p = T.constUse pnth_upp_obj p snth_upp_obj
      snth_upp_obj =
        T.constDef p a233v10nth_upp_obj
          (\ p ->
            T.ccase p233v26 p
              (let
                v233v26v1 (T.R Stop2 _) p =
                  T.ap1 p234v42 p (gavBottomR p234v42 p) fd
                v233v26v1 (T.R Up2 _) p =
                  T.ap1 p235v42 p (gavBottomR p235v42 p) fd
                v233v26v1 (T.R (UpUp2 frs) _) p =
                  T.ap2 p236v45 p (p236v45 !## p) frs fx
                v233v26v1 _ p = T.fatal p in (v233v26v1)) (gapp_res p233v31 p))
      in
      (T.ap3 p238v10 p (gfsApp p238v10 p) fas
        (T.ap2 p238v20 p (gdrop p238v20 p) fn fxs)
        (T.con1 p238v32 p HPoint aHPoint (gnth_upp_obj p238v39 p)))
  hfsApp _ _ _ p = T.fatal p
  

gfsEvalConst ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun (HExpr Naam) (T.Fun (T.List (HExpr Naam)) Route))

gfsEvalConst pfsEvalConst p =
  T.fun2 afsEvalConst pfsEvalConst p hfsEvalConst
  where
  
  hfsEvalConst (fh@(T.R (HLam _ _) _)) fxs p =
    T.ap2 p247v31 p (gaeEvalExact p247v31 p) fh fxs
  hfsEvalConst (fh@(T.R (HPoint fp) _)) (T.R T.List _) p =
    T.projection p248v31 p fp
  hfsEvalConst (fh@(T.R (HPoint _) _)) fxs p =
    T.ap1 p249v31 p (gaeEvalConst p249v31 p)
      (T.con2 p249v44 p HVAp aHVAp fh fxs)
  hfsEvalConst _ _ p = T.fatal p
  

gfsFs2 ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun ACMode
          (T.Fun (HExpr Naam)
            (T.Fun (T.List Domain)
              (T.Fun (T.List Domain)
                (T.Fun Frontier
                  (T.Fun (T.List AppInfo) (T.Fun Bool Frontier)))))))

gfsFs2 pfsFs2 p =
  T.fun7 afsFs2 pfsFs2 p hfsFs2
  where
  
  hfsFs2 fs_or_l fhexpr fsmall_argds fbig_argds
    (T.R (Min1Max0 far1 fmin1_init fmax0_init) _) fas fnaive p =
    let
      garity parity p = T.constUse parity p sarity
      sarity =
        T.constDef p a272v10arity
          (\ p -> T.ap1 p273v15 p (glength p273v15 p) fsmall_argds)
      ginitial_yy pinitial_yy p = T.constUse pinitial_yy p sinitial_yy
      sinitial_yy =
        T.constDef p a274v10initial_yy
          (\ p ->
            T.cif p275v15 p fnaive
              (\ p ->
                T.fromExpList p276v22 p
                  [T.con1 p276v23 p MkFrel aMkFrel
                      (T.ap2 p276v31 p (gmap p276v31 p) (gavTopR p276v35 p)
                        fsmall_argds)])
              (\ p -> T.projection p277v22 p fmax0_init))
      ginitial_xx pinitial_xx p = T.constUse pinitial_xx p sinitial_xx
      sinitial_xx =
        T.constDef p a278v10initial_xx
          (\ p ->
            T.cif p279v15 p fnaive
              (\ p ->
                T.fromExpList p280v22 p
                  [T.con1 p280v23 p MkFrel aMkFrel
                      (T.ap2 p280v31 p (gmap p280v31 p) (gavBottomR p280v35 p)
                        fsmall_argds)])
              (\ p -> T.projection p281v22 p fmin1_init))
      gfinal_yy pfinal_yy p = T.constUse pfinal_yy p sfinal_yy
      gfinal_xx pfinal_yy p = T.constUse pfinal_yy p sfinal_xx
      j282v10final_yy =
        case
          T.ap9 p283v15 p (gfsFs_aux p283v15 p) fs_or_l fhexpr fsmall_argds
            fbig_argds (ginitial_yy p287v24 p) (ginitial_xx p288v24 p) fas
            (T.con0 p290v24 p True aTrue)
            (T.ap2 p291v25 p (gutRandomInts p291v25 p)
              (T.ap1 p291v38 p (TPreludeBasic.gfromInteger p291v38 p)
                (T.conInteger p291v38 p 1))
              (T.ap1 p291v40 p (TPreludeBasic.gfromInteger p291v40 p)
                (T.conInteger p291v40 p 2))) of
          T.R (T.Tuple2 ffinal_yy ffinal_xx) kfinal_yy ->
            (kfinal_yy,ffinal_yy,ffinal_xx)
          _ -> T.fatal p
      sfinal_yy =
        T.constDef p a282v11final_yy
          (\ _ ->
            case j282v10final_yy of
              (kfinal_yy,ffinal_yy,ffinal_xx) ->
                T.projection p282v11 kfinal_yy ffinal_yy)
      sfinal_xx =
        T.constDef p a282v21final_xx
          (\ _ ->
            case j282v10final_yy of
              (kfinal_yy,ffinal_yy,ffinal_xx) ->
                T.projection p282v21 kfinal_yy ffinal_xx) in
      (T.con3 p293v10 p Min1Max0 aMin1Max0 (garity p293v19 p)
        (gfinal_xx p293v25 p) (gfinal_yy p293v34 p))
  hfsFs2 _ _ _ _ _ _ _ p = T.fatal p
  

gfsFs_aux ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun ACMode
          (T.Fun (HExpr Naam)
            (T.Fun (T.List Domain)
              (T.Fun (T.List Domain)
                (T.Fun (T.List FrontierElem)
                  (T.Fun (T.List FrontierElem)
                    (T.Fun (T.List AppInfo)
                      (T.Fun Bool
                        (T.Fun (T.List Int)
                          (T.Tuple2 (T.List FrontierElem)
                            (T.List FrontierElem)))))))))))

gfsFs_aux pfsFs_aux p =
  T.fun9 afsFs_aux pfsFs_aux p hfsFs_aux
  where
  
  hfsFs_aux fs_or_l fhexpr fsmall_argds fbig_argds ftrial_max_yy ftrial_min_xx
    fapp_info ffromTop frands p =
    let
      gedges pedges p = T.constUse pedges p sedges
      sedges =
        T.constDef p a322v10edges
          (\ p ->
            T.ap4 p323v15 p (gfmSelect p323v15 p)
              (T.ap1 p323v25 p (ghead p323v25 p) frands) ftrial_min_xx
              ftrial_max_yy ffromTop)
      gargs pargs p = T.constUse pargs p sargs
      j324v10args =
        case gedges p325v15 p of
          T.R (Just (T.R (MkFrel fargs) _)) kargs -> (kargs,fargs)
          _ -> T.fatal p
      sargs =
        T.constDef p a324v23args
          (\ _ ->
            case j324v10args of
              (kargs,fargs) -> T.projection p324v23 kargs fargs)
      gargs_at_proper_sizes pargs_at_proper_sizes p =
        T.constUse pargs_at_proper_sizes p sargs_at_proper_sizes
      sargs_at_proper_sizes =
        T.constDef p a326v10args_at_proper_sizes
          (\ p ->
            T.ap3 p327v15 p (gmakeBigger p327v15 p) (gargs p327v26 p)
              fsmall_argds fbig_argds)
      gevald_app pevald_app p = T.constUse pevald_app p sevald_app
      sevald_app =
        T.constDef p a328v10evald_app
          (\ p ->
            T.ap3 p329v15 p (gfsApp p329v15 p) fapp_info
              (T.ap2 p329v31 p (gmap p329v31 p)
                (T.pa0 HPoint T.cn1 p329v35 p aHPoint)
                (gargs_at_proper_sizes p329v42 p)) fhexpr)
      grevised_max_yy prevised_max_yy p =
        T.constUse prevised_max_yy p srevised_max_yy
      srevised_max_yy =
        T.constDef p a330v10revised_max_yy
          (\ p ->
            T.ap3 p331v15 p (gfmReviseMaxYY p331v15 p) fsmall_argds
              ftrial_max_yy (T.con1 p331v55 p MkFrel aMkFrel (gargs p331v62 p)))
      grevised_min_xx prevised_min_xx p =
        T.constUse prevised_min_xx p srevised_min_xx
      srevised_min_xx =
        T.constDef p a332v10revised_min_xx
          (\ p ->
            T.ap3 p333v15 p (gfmReviseMinXX p333v15 p) fsmall_argds
              ftrial_min_xx (T.con1 p333v55 p MkFrel aMkFrel (gargs p333v62 p)))
      gmakeBigger pmakeBigger p =
        T.fun3 a334v10makeBigger pmakeBigger p hmakeBigger
        where
        
        hmakeBigger frs (T.R T.List _) (T.R T.List _) p =
          T.projection p335v15 p frs
        hmakeBigger (T.R (T.Cons fr frs) _) (T.R (T.Cons fs fss) _)
          (T.R (T.Cons fb fbs) _) p =
          T.con2 p337v35 p T.Cons T.aCons
            (T.ap4 p337v15 p (gacConc p337v15 p) fs_or_l fb fs fr)
            (T.ap3 p337v37 p (gmakeBigger p337v37 p) frs fss fbs)
        hmakeBigger _ _ _ p = T.fatal p
         in
      (T.cif p339v10 p
        (T.ap1 p339v18 p (gfmIsNothing p339v18 p) (gedges p339v30 p))
        (\ p ->
          T.con2 p340v18 p T.Tuple2 T.aTuple2
            (T.ap1 p340v19 p (gsort p340v19 p) ftrial_max_yy)
            (T.ap1 p340v38 p (gsort p340v38 p) ftrial_min_xx))
        (\ p ->
          T.cif p342v10 p
            (T.ap2 p342v28 p (p342v28 !== p) (gevald_app p342v18 p)
              (T.con0 p342v31 p One aOne))
            (\ p ->
              T.ap9 p343v18 p (gfsFs_aux p343v18 p) fs_or_l fhexpr fsmall_argds
                fbig_argds (grevised_max_yy p347v27 p) ftrial_min_xx fapp_info
                (T.con0 p350v27 p False aFalse)
                (T.ap1 p351v28 p (gtail p351v28 p) frands))
            (\ p ->
              T.cif p353v10 p
                (T.ap2 p353v28 p (p353v28 !== p) (gevald_app p353v18 p)
                  (T.con0 p353v31 p Zero aZero))
                (\ p ->
                  T.ap9 p354v18 p (gfsFs_aux p354v18 p) fs_or_l fhexpr
                    fsmall_argds fbig_argds ftrial_max_yy
                    (grevised_min_xx p359v27 p) fapp_info
                    (T.con0 p361v27 p True aTrue)
                    (T.ap1 p362v28 p (gtail p362v28 p) frands))
                (\ p ->
                  T.ap1 p364v18 p (gpanic p364v18 p)
                    (T.fromLitString p364v24 p "fsFs_aux")))))
  

tFrontierGENERIC2 =
  T.mkModule "FrontierGENERIC2" "FrontierGENERIC2.hs" Prelude.True

afsMakeFrontierRep =
  T.mkVariable tFrontierGENERIC2 330001 3 7 "fsMakeFrontierRep" Prelude.False

afsFind = T.mkVariable tFrontierGENERIC2 890001 3 9 "fsFind" Prelude.False

afsApp = T.mkVariable tFrontierGENERIC2 2030001 3 3 "fsApp" Prelude.False

afsEvalConst =
  T.mkVariable tFrontierGENERIC2 2470001 3 2 "fsEvalConst" Prelude.False

afsFs2 = T.mkVariable tFrontierGENERIC2 2630001 3 7 "fsFs2" Prelude.False

afsFs_aux = T.mkVariable tFrontierGENERIC2 3100001 3 9 "fsFs_aux" Prelude.False

a36v11is_caf = T.mkVariable tFrontierGENERIC2 360011 3 0 "is_caf" Prelude.True

a36v19small_arg_ds =
  T.mkVariable tFrontierGENERIC2 360019 3 0 "small_arg_ds" Prelude.True

a41v10getRep = T.mkVariable tFrontierGENERIC2 410010 3 1 "getRep" Prelude.True

a43v10upper_bound =
  T.mkVariable tFrontierGENERIC2 430010 3 0 "upper_bound" Prelude.True

a45v10lower_bound =
  T.mkVariable tFrontierGENERIC2 450010 3 0 "lower_bound" Prelude.True

a47v10bound_rep =
  T.mkVariable tFrontierGENERIC2 470010 3 0 "bound_rep" Prelude.True

a49v10init_memo =
  T.mkVariable tFrontierGENERIC2 490010 3 0 "init_memo" Prelude.True

a51v10caf_result =
  T.mkVariable tFrontierGENERIC2 510010 3 0 "caf_result" Prelude.True

a53v10non_data_fn_result =
  T.mkVariable tFrontierGENERIC2 530010 3 0 "non_data_fn_result" Prelude.True

a56v11data_fn_result =
  T.mkVariable tFrontierGENERIC2 560011 3 0 "data_fn_result" Prelude.True

a56v27final_memo =
  T.mkVariable tFrontierGENERIC2 560027 3 0 "final_memo" Prelude.True

a60v10data_fn_evals =
  T.mkVariable tFrontierGENERIC2 600010 3 0 "data_fn_evals" Prelude.True

a62v10caf_result_norm =
  T.mkVariable tFrontierGENERIC2 620010 3 0 "caf_result_norm" Prelude.True

a64v10is_data_fn =
  T.mkVariable tFrontierGENERIC2 640010 3 0 "is_data_fn" Prelude.True

a115v10lofact = T.mkVariable tFrontierGENERIC2 1150010 3 0 "lofact" Prelude.True

a123v10hifact_ds =
  T.mkVariable tFrontierGENERIC2 1230010 3 0 "hifact_ds" Prelude.True

a125v10lofact_arity =
  T.mkVariable tFrontierGENERIC2 1250010 3 0 "lofact_arity" Prelude.True

a127v10hifacts =
  T.mkVariable tFrontierGENERIC2 1270010 3 0 "hifacts" Prelude.True

a133v10doOne = T.mkVariable tFrontierGENERIC2 1330010 3 4 "doOne" Prelude.True

a156v10lofact = T.mkVariable tFrontierGENERIC2 1560010 3 0 "lofact" Prelude.True

a164v10midfact =
  T.mkVariable tFrontierGENERIC2 1640010 3 0 "midfact" Prelude.True

a172v10hifact_ds =
  T.mkVariable tFrontierGENERIC2 1720010 3 0 "hifact_ds" Prelude.True

a174v10lofact_arity =
  T.mkVariable tFrontierGENERIC2 1740010 3 0 "lofact_arity" Prelude.True

a176v10hifacts =
  T.mkVariable tFrontierGENERIC2 1760010 3 0 "hifacts" Prelude.True

a182v10doOne = T.mkVariable tFrontierGENERIC2 1820010 3 4 "doOne" Prelude.True

a212v10app_res =
  T.mkVariable tFrontierGENERIC2 2120010 3 0 "app_res" Prelude.True

a213v10nth_upp_obj =
  T.mkVariable tFrontierGENERIC2 2130010 3 0 "nth_upp_obj" Prelude.True

a232v10app_res =
  T.mkVariable tFrontierGENERIC2 2320010 3 0 "app_res" Prelude.True

a233v10nth_upp_obj =
  T.mkVariable tFrontierGENERIC2 2330010 3 0 "nth_upp_obj" Prelude.True

a272v10arity = T.mkVariable tFrontierGENERIC2 2720010 3 0 "arity" Prelude.True

a274v10initial_yy =
  T.mkVariable tFrontierGENERIC2 2740010 3 0 "initial_yy" Prelude.True

a278v10initial_xx =
  T.mkVariable tFrontierGENERIC2 2780010 3 0 "initial_xx" Prelude.True

a282v11final_yy =
  T.mkVariable tFrontierGENERIC2 2820011 3 0 "final_yy" Prelude.True

a282v21final_xx =
  T.mkVariable tFrontierGENERIC2 2820021 3 0 "final_xx" Prelude.True

a322v10edges = T.mkVariable tFrontierGENERIC2 3220010 3 0 "edges" Prelude.True

a324v23args = T.mkVariable tFrontierGENERIC2 3240023 3 0 "args" Prelude.True

a326v10args_at_proper_sizes =
  T.mkVariable tFrontierGENERIC2 3260010 3 0 "args_at_proper_sizes" Prelude.True

a328v10evald_app =
  T.mkVariable tFrontierGENERIC2 3280010 3 0 "evald_app" Prelude.True

a330v10revised_max_yy =
  T.mkVariable tFrontierGENERIC2 3300010 3 0 "revised_max_yy" Prelude.True

a332v10revised_min_xx =
  T.mkVariable tFrontierGENERIC2 3320010 3 0 "revised_min_xx" Prelude.True

a334v10makeBigger =
  T.mkVariable tFrontierGENERIC2 3340010 3 3 "makeBigger" Prelude.True

p33v1 = T.mkSrcPos tFrontierGENERIC2 330001

p36v11 = T.mkSrcPos tFrontierGENERIC2 360011

p36v19 = T.mkSrcPos tFrontierGENERIC2 360019

p37v15 = T.mkSrcPos tFrontierGENERIC2 370015

p38v39 = T.mkSrcPos tFrontierGENERIC2 380039

p38v40 = T.mkSrcPos tFrontierGENERIC2 380040

p38v46 = T.mkSrcPos tFrontierGENERIC2 380046

p38v52 = T.mkSrcPos tFrontierGENERIC2 380052

p39v39 = T.mkSrcPos tFrontierGENERIC2 390039

p39v40 = T.mkSrcPos tFrontierGENERIC2 390040

p40v39 = T.mkSrcPos tFrontierGENERIC2 400039

p40v40 = T.mkSrcPos tFrontierGENERIC2 400040

p40v46 = T.mkSrcPos tFrontierGENERIC2 400046

p40v52 = T.mkSrcPos tFrontierGENERIC2 400052

p41v10 = T.mkSrcPos tFrontierGENERIC2 410010

p42v15 = T.mkSrcPos tFrontierGENERIC2 420015

p43v10 = T.mkSrcPos tFrontierGENERIC2 430010

p44v15 = T.mkSrcPos tFrontierGENERIC2 440015

p45v10 = T.mkSrcPos tFrontierGENERIC2 450010

p46v15 = T.mkSrcPos tFrontierGENERIC2 460015

p47v10 = T.mkSrcPos tFrontierGENERIC2 470010

p48v15 = T.mkSrcPos tFrontierGENERIC2 480015

p48v22 = T.mkSrcPos tFrontierGENERIC2 480022

p48v34 = T.mkSrcPos tFrontierGENERIC2 480034

p49v10 = T.mkSrcPos tFrontierGENERIC2 490010

p50v15 = T.mkSrcPos tFrontierGENERIC2 500015

p51v10 = T.mkSrcPos tFrontierGENERIC2 510010

p52v15 = T.mkSrcPos tFrontierGENERIC2 520015

p53v10 = T.mkSrcPos tFrontierGENERIC2 530010

p54v15 = T.mkSrcPos tFrontierGENERIC2 540015

p55v22 = T.mkSrcPos tFrontierGENERIC2 550022

p55v46 = T.mkSrcPos tFrontierGENERIC2 550046

p55v56 = T.mkSrcPos tFrontierGENERIC2 550056

p55v58 = T.mkSrcPos tFrontierGENERIC2 550058

p56v11 = T.mkSrcPos tFrontierGENERIC2 560011

p56v27 = T.mkSrcPos tFrontierGENERIC2 560027

p57v15 = T.mkSrcPos tFrontierGENERIC2 570015

p58v22 = T.mkSrcPos tFrontierGENERIC2 580022

p58v46 = T.mkSrcPos tFrontierGENERIC2 580046

p58v56 = T.mkSrcPos tFrontierGENERIC2 580056

p59v23 = T.mkSrcPos tFrontierGENERIC2 590023

p59v29 = T.mkSrcPos tFrontierGENERIC2 590029

p59v50 = T.mkSrcPos tFrontierGENERIC2 590050

p60v10 = T.mkSrcPos tFrontierGENERIC2 600010

p61v15 = T.mkSrcPos tFrontierGENERIC2 610015

p61v22 = T.mkSrcPos tFrontierGENERIC2 610022

p62v10 = T.mkSrcPos tFrontierGENERIC2 620010

p63v15 = T.mkSrcPos tFrontierGENERIC2 630015

p63v20 = T.mkSrcPos tFrontierGENERIC2 630020

p63v46 = T.mkSrcPos tFrontierGENERIC2 630046

p63v71 = T.mkSrcPos tFrontierGENERIC2 630071

p64v10 = T.mkSrcPos tFrontierGENERIC2 640010

p65v15 = T.mkSrcPos tFrontierGENERIC2 650015

p67v10 = T.mkSrcPos tFrontierGENERIC2 670010

p67v17 = T.mkSrcPos tFrontierGENERIC2 670017

p68v17 = T.mkSrcPos tFrontierGENERIC2 680017

p68v18 = T.mkSrcPos tFrontierGENERIC2 680018

p68v35 = T.mkSrcPos tFrontierGENERIC2 680035

p70v10 = T.mkSrcPos tFrontierGENERIC2 700010

p70v17 = T.mkSrcPos tFrontierGENERIC2 700017

p71v17 = T.mkSrcPos tFrontierGENERIC2 710017

p71v18 = T.mkSrcPos tFrontierGENERIC2 710018

p71v22 = T.mkSrcPos tFrontierGENERIC2 710022

p71v38 = T.mkSrcPos tFrontierGENERIC2 710038

p72v17 = T.mkSrcPos tFrontierGENERIC2 720017

p72v18 = T.mkSrcPos tFrontierGENERIC2 720018

p72v22 = T.mkSrcPos tFrontierGENERIC2 720022

p72v43 = T.mkSrcPos tFrontierGENERIC2 720043

p72v44 = T.mkSrcPos tFrontierGENERIC2 720044

p89v1 = T.mkSrcPos tFrontierGENERIC2 890001

p97v6 = T.mkSrcPos tFrontierGENERIC2 970006

p97v14 = T.mkSrcPos tFrontierGENERIC2 970014

p102v23 = T.mkSrcPos tFrontierGENERIC2 1020023

p102v25 = T.mkSrcPos tFrontierGENERIC2 1020025

p102v26 = T.mkSrcPos tFrontierGENERIC2 1020026

p115v10 = T.mkSrcPos tFrontierGENERIC2 1150010

p116v15 = T.mkSrcPos tFrontierGENERIC2 1160015

p121v24 = T.mkSrcPos tFrontierGENERIC2 1210024

p121v26 = T.mkSrcPos tFrontierGENERIC2 1210026

p121v27 = T.mkSrcPos tFrontierGENERIC2 1210027

p123v10 = T.mkSrcPos tFrontierGENERIC2 1230010

p124v15 = T.mkSrcPos tFrontierGENERIC2 1240015

p124v20 = T.mkSrcPos tFrontierGENERIC2 1240020

p125v10 = T.mkSrcPos tFrontierGENERIC2 1250010

p126v15 = T.mkSrcPos tFrontierGENERIC2 1260015

p127v10 = T.mkSrcPos tFrontierGENERIC2 1270010

p128v15 = T.mkSrcPos tFrontierGENERIC2 1280015

p128v26 = T.mkSrcPos tFrontierGENERIC2 1280026

p129v26 = T.mkSrcPos tFrontierGENERIC2 1290026

p132v30 = T.mkSrcPos tFrontierGENERIC2 1320030

p132v27 = T.mkSrcPos tFrontierGENERIC2 1320027

p132v56 = T.mkSrcPos tFrontierGENERIC2 1320056

p132v45 = T.mkSrcPos tFrontierGENERIC2 1320045

p132v58 = T.mkSrcPos tFrontierGENERIC2 1320058

p133v10 = T.mkSrcPos tFrontierGENERIC2 1330010

p134v15 = T.mkSrcPos tFrontierGENERIC2 1340015

p140v22 = T.mkSrcPos tFrontierGENERIC2 1400022

p141v25 = T.mkSrcPos tFrontierGENERIC2 1410025

p141v27 = T.mkSrcPos tFrontierGENERIC2 1410027

p141v28 = T.mkSrcPos tFrontierGENERIC2 1410028

p141v33 = T.mkSrcPos tFrontierGENERIC2 1410033

p144v10 = T.mkSrcPos tFrontierGENERIC2 1440010

p144v15 = T.mkSrcPos tFrontierGENERIC2 1440015

p144v22 = T.mkSrcPos tFrontierGENERIC2 1440022

p156v10 = T.mkSrcPos tFrontierGENERIC2 1560010

p157v15 = T.mkSrcPos tFrontierGENERIC2 1570015

p162v24 = T.mkSrcPos tFrontierGENERIC2 1620024

p162v26 = T.mkSrcPos tFrontierGENERIC2 1620026

p162v27 = T.mkSrcPos tFrontierGENERIC2 1620027

p164v10 = T.mkSrcPos tFrontierGENERIC2 1640010

p165v15 = T.mkSrcPos tFrontierGENERIC2 1650015

p170v24 = T.mkSrcPos tFrontierGENERIC2 1700024

p170v26 = T.mkSrcPos tFrontierGENERIC2 1700026

p170v27 = T.mkSrcPos tFrontierGENERIC2 1700027

p172v10 = T.mkSrcPos tFrontierGENERIC2 1720010

p173v15 = T.mkSrcPos tFrontierGENERIC2 1730015

p173v20 = T.mkSrcPos tFrontierGENERIC2 1730020

p174v10 = T.mkSrcPos tFrontierGENERIC2 1740010

p175v15 = T.mkSrcPos tFrontierGENERIC2 1750015

p176v10 = T.mkSrcPos tFrontierGENERIC2 1760010

p177v15 = T.mkSrcPos tFrontierGENERIC2 1770015

p177v26 = T.mkSrcPos tFrontierGENERIC2 1770026

p178v26 = T.mkSrcPos tFrontierGENERIC2 1780026

p181v30 = T.mkSrcPos tFrontierGENERIC2 1810030

p181v27 = T.mkSrcPos tFrontierGENERIC2 1810027

p181v56 = T.mkSrcPos tFrontierGENERIC2 1810056

p181v45 = T.mkSrcPos tFrontierGENERIC2 1810045

p181v58 = T.mkSrcPos tFrontierGENERIC2 1810058

p182v10 = T.mkSrcPos tFrontierGENERIC2 1820010

p183v15 = T.mkSrcPos tFrontierGENERIC2 1830015

p189v22 = T.mkSrcPos tFrontierGENERIC2 1890022

p190v25 = T.mkSrcPos tFrontierGENERIC2 1900025

p190v27 = T.mkSrcPos tFrontierGENERIC2 1900027

p190v28 = T.mkSrcPos tFrontierGENERIC2 1900028

p190v33 = T.mkSrcPos tFrontierGENERIC2 1900033

p193v10 = T.mkSrcPos tFrontierGENERIC2 1930010

p193v15 = T.mkSrcPos tFrontierGENERIC2 1930015

p193v22 = T.mkSrcPos tFrontierGENERIC2 1930022

p193v30 = T.mkSrcPos tFrontierGENERIC2 1930030

p203v1 = T.mkSrcPos tFrontierGENERIC2 2030001

p204v6 = T.mkSrcPos tFrontierGENERIC2 2040006

p207v6 = T.mkSrcPos tFrontierGENERIC2 2070006

p207v11 = T.mkSrcPos tFrontierGENERIC2 2070011

p208v19 = T.mkSrcPos tFrontierGENERIC2 2080019

p209v19 = T.mkSrcPos tFrontierGENERIC2 2090019

p212v10 = T.mkSrcPos tFrontierGENERIC2 2120010

p212v26 = T.mkSrcPos tFrontierGENERIC2 2120026

p212v41 = T.mkSrcPos tFrontierGENERIC2 2120041

p213v10 = T.mkSrcPos tFrontierGENERIC2 2130010

p213v26 = T.mkSrcPos tFrontierGENERIC2 2130026

p213v31 = T.mkSrcPos tFrontierGENERIC2 2130031

p214v40 = T.mkSrcPos tFrontierGENERIC2 2140040

p215v43 = T.mkSrcPos tFrontierGENERIC2 2150043

p217v10 = T.mkSrcPos tFrontierGENERIC2 2170010

p217v20 = T.mkSrcPos tFrontierGENERIC2 2170020

p217v32 = T.mkSrcPos tFrontierGENERIC2 2170032

p217v39 = T.mkSrcPos tFrontierGENERIC2 2170039

p220v6 = T.mkSrcPos tFrontierGENERIC2 2200006

p220v11 = T.mkSrcPos tFrontierGENERIC2 2200011

p221v21 = T.mkSrcPos tFrontierGENERIC2 2210021

p222v21 = T.mkSrcPos tFrontierGENERIC2 2220021

p223v21 = T.mkSrcPos tFrontierGENERIC2 2230021

p226v6 = T.mkSrcPos tFrontierGENERIC2 2260006

p226v11 = T.mkSrcPos tFrontierGENERIC2 2260011

p227v21 = T.mkSrcPos tFrontierGENERIC2 2270021

p228v21 = T.mkSrcPos tFrontierGENERIC2 2280021

p229v21 = T.mkSrcPos tFrontierGENERIC2 2290021

p232v10 = T.mkSrcPos tFrontierGENERIC2 2320010

p232v26 = T.mkSrcPos tFrontierGENERIC2 2320026

p232v41 = T.mkSrcPos tFrontierGENERIC2 2320041

p233v10 = T.mkSrcPos tFrontierGENERIC2 2330010

p233v26 = T.mkSrcPos tFrontierGENERIC2 2330026

p233v31 = T.mkSrcPos tFrontierGENERIC2 2330031

p234v42 = T.mkSrcPos tFrontierGENERIC2 2340042

p235v42 = T.mkSrcPos tFrontierGENERIC2 2350042

p236v45 = T.mkSrcPos tFrontierGENERIC2 2360045

p238v10 = T.mkSrcPos tFrontierGENERIC2 2380010

p238v20 = T.mkSrcPos tFrontierGENERIC2 2380020

p238v32 = T.mkSrcPos tFrontierGENERIC2 2380032

p238v39 = T.mkSrcPos tFrontierGENERIC2 2380039

p247v1 = T.mkSrcPos tFrontierGENERIC2 2470001

p247v31 = T.mkSrcPos tFrontierGENERIC2 2470031

p248v31 = T.mkSrcPos tFrontierGENERIC2 2480031

p249v31 = T.mkSrcPos tFrontierGENERIC2 2490031

p249v44 = T.mkSrcPos tFrontierGENERIC2 2490044

p263v1 = T.mkSrcPos tFrontierGENERIC2 2630001

p272v10 = T.mkSrcPos tFrontierGENERIC2 2720010

p273v15 = T.mkSrcPos tFrontierGENERIC2 2730015

p274v10 = T.mkSrcPos tFrontierGENERIC2 2740010

p275v15 = T.mkSrcPos tFrontierGENERIC2 2750015

p276v22 = T.mkSrcPos tFrontierGENERIC2 2760022

p276v23 = T.mkSrcPos tFrontierGENERIC2 2760023

p276v31 = T.mkSrcPos tFrontierGENERIC2 2760031

p276v35 = T.mkSrcPos tFrontierGENERIC2 2760035

p277v22 = T.mkSrcPos tFrontierGENERIC2 2770022

p278v10 = T.mkSrcPos tFrontierGENERIC2 2780010

p279v15 = T.mkSrcPos tFrontierGENERIC2 2790015

p280v22 = T.mkSrcPos tFrontierGENERIC2 2800022

p280v23 = T.mkSrcPos tFrontierGENERIC2 2800023

p280v31 = T.mkSrcPos tFrontierGENERIC2 2800031

p280v35 = T.mkSrcPos tFrontierGENERIC2 2800035

p281v22 = T.mkSrcPos tFrontierGENERIC2 2810022

p282v11 = T.mkSrcPos tFrontierGENERIC2 2820011

p282v21 = T.mkSrcPos tFrontierGENERIC2 2820021

p283v15 = T.mkSrcPos tFrontierGENERIC2 2830015

p287v24 = T.mkSrcPos tFrontierGENERIC2 2870024

p288v24 = T.mkSrcPos tFrontierGENERIC2 2880024

p290v24 = T.mkSrcPos tFrontierGENERIC2 2900024

p291v25 = T.mkSrcPos tFrontierGENERIC2 2910025

p291v38 = T.mkSrcPos tFrontierGENERIC2 2910038

p291v40 = T.mkSrcPos tFrontierGENERIC2 2910040

p293v10 = T.mkSrcPos tFrontierGENERIC2 2930010

p293v19 = T.mkSrcPos tFrontierGENERIC2 2930019

p293v25 = T.mkSrcPos tFrontierGENERIC2 2930025

p293v34 = T.mkSrcPos tFrontierGENERIC2 2930034

p310v1 = T.mkSrcPos tFrontierGENERIC2 3100001

p322v10 = T.mkSrcPos tFrontierGENERIC2 3220010

p323v15 = T.mkSrcPos tFrontierGENERIC2 3230015

p323v25 = T.mkSrcPos tFrontierGENERIC2 3230025

p324v23 = T.mkSrcPos tFrontierGENERIC2 3240023

p325v15 = T.mkSrcPos tFrontierGENERIC2 3250015

p326v10 = T.mkSrcPos tFrontierGENERIC2 3260010

p327v15 = T.mkSrcPos tFrontierGENERIC2 3270015

p327v26 = T.mkSrcPos tFrontierGENERIC2 3270026

p328v10 = T.mkSrcPos tFrontierGENERIC2 3280010

p329v15 = T.mkSrcPos tFrontierGENERIC2 3290015

p329v31 = T.mkSrcPos tFrontierGENERIC2 3290031

p329v35 = T.mkSrcPos tFrontierGENERIC2 3290035

p329v42 = T.mkSrcPos tFrontierGENERIC2 3290042

p330v10 = T.mkSrcPos tFrontierGENERIC2 3300010

p331v15 = T.mkSrcPos tFrontierGENERIC2 3310015

p331v55 = T.mkSrcPos tFrontierGENERIC2 3310055

p331v62 = T.mkSrcPos tFrontierGENERIC2 3310062

p332v10 = T.mkSrcPos tFrontierGENERIC2 3320010

p333v15 = T.mkSrcPos tFrontierGENERIC2 3330015

p333v55 = T.mkSrcPos tFrontierGENERIC2 3330055

p333v62 = T.mkSrcPos tFrontierGENERIC2 3330062

p334v10 = T.mkSrcPos tFrontierGENERIC2 3340010

p335v15 = T.mkSrcPos tFrontierGENERIC2 3350015

p337v35 = T.mkSrcPos tFrontierGENERIC2 3370035

p337v15 = T.mkSrcPos tFrontierGENERIC2 3370015

p337v37 = T.mkSrcPos tFrontierGENERIC2 3370037

p339v10 = T.mkSrcPos tFrontierGENERIC2 3390010

p339v18 = T.mkSrcPos tFrontierGENERIC2 3390018

p339v30 = T.mkSrcPos tFrontierGENERIC2 3390030

p340v18 = T.mkSrcPos tFrontierGENERIC2 3400018

p340v19 = T.mkSrcPos tFrontierGENERIC2 3400019

p340v38 = T.mkSrcPos tFrontierGENERIC2 3400038

p342v10 = T.mkSrcPos tFrontierGENERIC2 3420010

p342v28 = T.mkSrcPos tFrontierGENERIC2 3420028

p342v18 = T.mkSrcPos tFrontierGENERIC2 3420018

p342v31 = T.mkSrcPos tFrontierGENERIC2 3420031

p343v18 = T.mkSrcPos tFrontierGENERIC2 3430018

p347v27 = T.mkSrcPos tFrontierGENERIC2 3470027

p350v27 = T.mkSrcPos tFrontierGENERIC2 3500027

p351v28 = T.mkSrcPos tFrontierGENERIC2 3510028

p353v10 = T.mkSrcPos tFrontierGENERIC2 3530010

p353v28 = T.mkSrcPos tFrontierGENERIC2 3530028

p353v18 = T.mkSrcPos tFrontierGENERIC2 3530018

p353v31 = T.mkSrcPos tFrontierGENERIC2 3530031

p354v18 = T.mkSrcPos tFrontierGENERIC2 3540018

p359v27 = T.mkSrcPos tFrontierGENERIC2 3590027

p361v27 = T.mkSrcPos tFrontierGENERIC2 3610027

p362v28 = T.mkSrcPos tFrontierGENERIC2 3620028

p364v18 = T.mkSrcPos tFrontierGENERIC2 3640018

p364v24 = T.mkSrcPos tFrontierGENERIC2 3640024
